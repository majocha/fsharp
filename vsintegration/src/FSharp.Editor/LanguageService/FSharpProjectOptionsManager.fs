// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Collections.Immutable
open System.IO
open System.Linq
open Microsoft.CodeAnalysis
open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open Microsoft.VisualStudio.FSharp.Editor
open System.Threading
open Microsoft.VisualStudio.FSharp.Interactive.Session
open System.Runtime.CompilerServices
open CancellableTasks
open Microsoft.VisualStudio.FSharp.Editor.Extensions
open System.Windows
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell
open FSharp.Compiler.Text
open Microsoft.VisualStudio.TextManager.Interop

open Internal.Utilities.Collections

#nowarn "57"

[<AutoOpen>]
module private FSharpProjectOptionsHelpers =

    let projectOptionsCache = ConcurrentDictionary<ProjectId, AsyncLazy<FSharpParsingOptions * FSharpProjectOptions>>()

    let mapCpsProjectToSite (project: Project, sourcePaths, options) =
        let mutable errorReporter = Unchecked.defaultof<_>

        { new IProjectSite with
            member _.Description = project.Name
            member _.CompilationSourceFiles = sourcePaths

            member _.CompilationOptions = options

            member _.CompilationReferences = [| |]

            member site.CompilationBinOutputPath =
                site.CompilationOptions
                |> Array.tryPick (fun s -> if s.StartsWith("-o:") then Some s.[3..] else None)

            member _.ProjectFileName = project.FilePath
            member _.AdviseProjectSiteChanges(_, _) = ()
            member _.AdviseProjectSiteCleaned(_, _) = ()
            member _.AdviseProjectSiteClosed(_, _) = ()
            member _.IsIncompleteTypeCheckEnvironment = false
            member _.TargetFrameworkMoniker = ""
            member _.ProjectGuid = project.Id.Id.ToString()
            member _.LoadTime = System.DateTime.Now
            member _.ProjectProvider = None

            member _.BuildErrorReporter
                with get () = errorReporter
                and set (v) = errorReporter <- v
        }

    let inline hasProjectVersionChanged (oldProject: Project) (newProject: Project) =
        oldProject.Version <> newProject.Version

    let hasDependentVersionChanged (oldProject: Project) (newProject: Project) (ct: CancellationToken) =
        let oldProjectMetadataRefs = oldProject.MetadataReferences
        let newProjectMetadataRefs = newProject.MetadataReferences

        if oldProjectMetadataRefs.Count <> newProjectMetadataRefs.Count then
            true
        else

            let oldProjectRefs = oldProject.ProjectReferences
            let newProjectRefs = newProject.ProjectReferences

            oldProjectRefs.Count() <> newProjectRefs.Count()
            || (oldProjectRefs, newProjectRefs)
               ||> Seq.exists2 (fun p1 p2 ->
                   ct.ThrowIfCancellationRequested()
                   let doesProjectIdDiffer = p1.ProjectId <> p2.ProjectId
                   let p1 = oldProject.Solution.GetProject(p1.ProjectId)
                   let p2 = newProject.Solution.GetProject(p2.ProjectId)

                   doesProjectIdDiffer
                   || (if p1.IsFSharp then
                           p1.Version <> p2.Version
                       else
                           let v1 = p1.GetDependentVersionAsync(ct).Result
                           let v2 = p2.GetDependentVersionAsync(ct).Result
                           v1 <> v2))

    let isProjectInvalidated (oldProject: Project) (newProject: Project) ct =
        let hasProjectVersionChanged = hasProjectVersionChanged oldProject newProject

        if newProject.AreFSharpInMemoryCrossProjectReferencesEnabled then
            hasProjectVersionChanged || hasDependentVersionChanged oldProject newProject ct
        else
            hasProjectVersionChanged



/// Manages mappings of Roslyn workspace Projects/Documents to FCS.
type internal FSharpProjectOptionsManager(checker: FSharpChecker, workspace: Workspace) =

    
    // Store command line options
    let commandLineOptions = ConcurrentDictionary<ProjectId, string[] * string[]>()

    let legacyProjectSites = ConcurrentDictionary<ProjectId, IProjectSite>()

    let cache =
        ConcurrentDictionary<ProjectId, Project * FSharpParsingOptions * FSharpProjectOptions>()

    let singleFileCache =
        ConcurrentDictionary<DocumentId, Project * VersionStamp * FSharpParsingOptions * FSharpProjectOptions * ConnectionPointSubscription>()

    // This is used to not constantly emit the same compilation.
    let weakPEReferences = ConditionalWeakTable<Compilation, FSharpReferencedProject>()
    let lastSuccessfulCompilations = ConcurrentDictionary<ProjectId, Compilation>()

    let scriptUpdatedEvent = Event<FSharpProjectOptions>()

    let createPEReference (referencedProject: Project) (comp: Compilation) =
        let projectId = referencedProject.Id

        match weakPEReferences.TryGetValue comp with
        | true, fsRefProj -> fsRefProj
        | _ ->
            let mutable strongComp = comp
            let weakComp = WeakReference<Compilation>(comp)
            let mutable stamp = DateTime.UtcNow

            // Getting a C# reference assembly can fail if there are compilation errors that cannot be resolved.
            // To mitigate this, we store the last successful compilation of a C# project and re-use it until we get a new successful compilation.
            let getStream =
                fun ct ->
                    let tryStream (comp: Compilation) =
                        let ms = new MemoryStream() // do not dispose the stream as it will be owned on the reference.

                        let emitOptions =
                            Emit.EmitOptions(metadataOnly = true, includePrivateMembers = false, tolerateErrors = true)

                        try
                            let result = comp.Emit(ms, options = emitOptions, cancellationToken = ct)

                            if result.Success then
                                strongComp <- Unchecked.defaultof<_> // Stop strongly holding the compilation since we have a result.
                                lastSuccessfulCompilations.[projectId] <- comp
                                ms.Position <- 0L
                                ms :> Stream |> Some
                            else
                                strongComp <- Unchecked.defaultof<_> // Stop strongly holding the compilation since we have a result.
                                ms.Dispose() // it failed, dispose of stream
                                None
                        with
                        | :? OperationCanceledException ->
                            // Since we cancelled, do not null out the strong compilation ref and update the stamp.
                            stamp <- DateTime.UtcNow
                            ms.Dispose()
                            None
                        | _ ->
                            strongComp <- Unchecked.defaultof<_> // Stop strongly holding the compilation since we have a result.
                            ms.Dispose() // it failed, dispose of stream
                            None

                    let resultOpt =
                        match weakComp.TryGetTarget() with
                        | true, comp -> tryStream comp
                        | _ -> None

                    match resultOpt with
                    | Some _ -> resultOpt
                    | _ ->
                        match lastSuccessfulCompilations.TryGetValue(projectId) with
                        | true, comp -> tryStream comp
                        | _ -> None

            let getStamp = fun () -> stamp

            let fsRefProj =
                FSharpReferencedProject.PEReference(getStamp, DelayedILModuleReader(referencedProject.OutputFilePath, getStream))

            weakPEReferences.Add(comp, fsRefProj)
            fsRefProj

    let rec tryComputeOptionsBySingleScriptOrFile (document: Document) userOpName =
        cancellableTask {
            let! ct = CancellableTask.getCancellationToken ()
            let! fileStamp = document.GetTextVersionAsync(ct)
            let textViewAndCaret () : (IVsTextView * Position) option = document.TryGetTextViewAndCaretPos()

            match singleFileCache.TryGetValue(document.Id) with
            | false, _ ->
                let! sourceText = document.GetTextAsync(ct)

                let getProjectOptionsFromScript textViewAndCaret =
                    let caret = textViewAndCaret ()

                    match caret with
                    | None ->
                        checker.GetProjectOptionsFromScript(
                            document.FilePath,
                            sourceText.ToFSharpSourceText(),
                            previewEnabled = SessionsProperties.fsiPreview,
                            assumeDotNetFramework = not SessionsProperties.fsiUseNetCore,
                            userOpName = userOpName
                        )

                    | Some(_, caret) ->
                        checker.GetProjectOptionsFromScript(
                            document.FilePath,
                            sourceText.ToFSharpSourceText(),
                            caret,
                            previewEnabled = SessionsProperties.fsiPreview,
                            assumeDotNetFramework = not SessionsProperties.fsiUseNetCore,
                            userOpName = userOpName
                        )

                let! scriptProjectOptions, _ = getProjectOptionsFromScript textViewAndCaret
                let project = document.Project

                let otherOptions =
                    if project.IsFSharpMetadata then
                        project.ProjectReferences
                        |> Seq.map (fun x -> "-r:" + project.Solution.GetProject(x.ProjectId).OutputFilePath)
                        |> Array.ofSeq
                        |> Array.append (
                            project.MetadataReferences.OfType<PortableExecutableReference>()
                            |> Seq.map (fun x -> "-r:" + x.FilePath)
                            |> Array.ofSeq
                        )
                    else
                        [||]

                let projectOptions =
                    if isScriptFile document.FilePath then
                        scriptUpdatedEvent.Trigger(scriptProjectOptions)
                        scriptProjectOptions
                    else
                        {
                            ProjectFileName = document.FilePath
                            ProjectId = None
                            SourceFiles = [| document.FilePath |]
                            OtherOptions = otherOptions
                            ReferencedProjects = [||]
                            IsIncompleteTypeCheckEnvironment = false
                            UseScriptResolutionRules = CompilerEnvironment.MustBeSingleFileProject(Path.GetFileName(document.FilePath))
                            LoadTime = DateTime.Now
                            UnresolvedReferences = None
                            OriginalLoadReferences = []
                            Stamp = Some(int64 (fileStamp.GetHashCode()))
                        }

                let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions(projectOptions)

                let updateProjectOptions () =
                    async {
                        let! scriptProjectOptions, _ = getProjectOptionsFromScript textViewAndCaret

                        checker.NotifyFileChanged(document.FilePath, scriptProjectOptions)
                        |> Async.Start
                    }
                    |> Async.Start

                let onChangeCaretHandler (_, _newline: int, _oldline: int) = updateProjectOptions ()
                let onKillFocus (_) = updateProjectOptions ()
                let onSetFocus (_) = updateProjectOptions ()

                let addToCacheAndSubscribe value =
                    match value with
                    | projectId, fileStamp, parsingOptions, projectOptions, _ ->
                        let subscription =
                            match textViewAndCaret () with
                            | Some(textView, _) ->
                                subscribeToTextViewEvents (textView, (Some onChangeCaretHandler), (Some onKillFocus), (Some onSetFocus))
                            | None -> None

                        (projectId, fileStamp, parsingOptions, projectOptions, subscription)

                singleFileCache.AddOrUpdate(
                    document.Id, // The key to the cache
                    (fun _ value -> addToCacheAndSubscribe value), // Function to add the cached value if the key does not exist
                    (fun _ _ value -> value), // Function to update the value if the key exists
                    (document.Project, fileStamp, parsingOptions, projectOptions, None) // The value to add or update
                )
                |> ignore

                return ValueSome(parsingOptions, projectOptions)

            | true, (oldProject, oldFileStamp, parsingOptions, projectOptions, _) ->
                if fileStamp <> oldFileStamp || isProjectInvalidated document.Project oldProject ct then
                    match singleFileCache.TryRemove(document.Id) with
                    | true, (_, _, _, _, Some subscription) -> subscription.Dispose()
                    | _ -> ()

                    return! tryComputeOptionsBySingleScriptOrFile document userOpName
                else
                    return ValueSome(parsingOptions, projectOptions)
        }

    let rec waitForOptions (project: Project) =
        async {
            match projectOptionsCache.TryGetValue(project.Id) with
            | true, compute -> return! compute.Request()
            | _ -> // return failwith "not waiting for this"
                do! Async.Sleep 100
                return! waitForOptions project
        }

    let getProjectSite (project: Project) sourcePaths options =
        async {

            match legacyProjectSites.TryGetValue project.Id with
            | true, site -> return site
            | _ ->
                return mapCpsProjectToSite (project, sourcePaths, options)
        }

    let computeProjectOptions (project: Project) (sourcePaths: string[]) (options: string[]) =
        async {
            let referencedProjects = ResizeArray()

            if project.AreFSharpInMemoryCrossProjectReferencesEnabled then
                for projectReference in project.ProjectReferences do
                    let referencedProject = project.Solution.GetProject(projectReference.ProjectId)

                    if referencedProject.Language = FSharpConstants.FSharpLanguageName then
                        let! _, referencedProjectOptions = waitForOptions referencedProject
                        referencedProjects.Add(
                            FSharpReferencedProject.FSharpReference(referencedProject.OutputFilePath, referencedProjectOptions)
                        )
                    elif referencedProject.SupportsCompilation then
                        let! ct = Async.CancellationToken
                        let! comp = referencedProject.GetCompilationAsync(ct) |> Async.AwaitTask
                        let peRef = createPEReference referencedProject comp
                        referencedProjects.Add(peRef)

            let! projectSite = getProjectSite project sourcePaths options

            let otherOptions =
                [|
                    // Clear any references from CompilationOptions.
                    // We get the references from Project.ProjectReferences/Project.MetadataReferences.
                    for x in projectSite.CompilationOptions do
                        if not (x.Contains("-r:")) then
                            x

                    for x in project.MetadataReferences.OfType<PortableExecutableReference>() do
                        "-r:" + x.FilePath

                    for x in project.ProjectReferences do
                        "-r:" + project.Solution.GetProject(x.ProjectId).OutputFilePath

                    // In the IDE we always ignore all #line directives for all purposes.  This means
                    // IDE features work correctly within generated source files, but diagnostics are
                    // reported in the IDE with respect to the generated source, and will not unify with
                    // diagnostics from the build.
                    "--ignorelinedirectives"
                |]

            let! ct = Async.CancellationToken
            let! ver = project.GetDependentVersionAsync(ct) |> Async.AwaitTask

            let projectOptions =
                {
                    ProjectFileName = projectSite.ProjectFileName
                    ProjectId = Some(project.Id.ToFSharpProjectIdString())
                    SourceFiles = projectSite.CompilationSourceFiles
                    OtherOptions = otherOptions
                    ReferencedProjects = referencedProjects.ToArray()
                    IsIncompleteTypeCheckEnvironment = projectSite.IsIncompleteTypeCheckEnvironment
                    UseScriptResolutionRules = CompilerEnvironment.MustBeSingleFileProject(Path.GetFileName(project.FilePath))
                    LoadTime = projectSite.LoadTime
                    UnresolvedReferences = None
                    OriginalLoadReferences = []
                    Stamp = Some(int64 (ver.GetHashCode()))
                }

            checker.ClearCache([ projectOptions ], userOpName = "tryComputeOptions")

            //lastSuccessfulCompilations.ToArray()
            //|> Array.iter (fun pair ->
            //    if not (currentSolution.ContainsProject(pair.Key)) then
            //        lastSuccessfulCompilations.TryRemove(pair.Key) |> ignore)

            checker.InvalidateConfiguration(projectOptions, userOpName = "tryComputeOptions")

            let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions(projectOptions)

            return parsingOptions, projectOptions
        }
  
    member _.GetOptionsByProject(project) = waitForOptions project

    member this.GetOptionsByDocument(document: Document) = waitForOptions document.Project
    member this.GetOptionsByDocumentId(documentId: DocumentId) =
        let document = workspace.CurrentSolution.GetDocument(documentId)
        ThreadHelper.ThrowIfOnUIThread()
        waitForOptions document.Project |> Async.RunImmediateExceptOnUI

    member _.SetCommandLineOptions(project: Project, sourcePaths, options) =
        projectOptionsCache[project.Id] <- AsyncLazy((computeProjectOptions project sourcePaths options), cancelUnawaited = true, cacheException = false) 

    member _.SetLegacyProjectSite(projectId, projectSite) =
        legacyProjectSites.[projectId] <- projectSite

    member _.ClearAllCaches() =
        commandLineOptions.Clear()
        legacyProjectSites.Clear()
        cache.Clear()
        singleFileCache.Clear()
        lastSuccessfulCompilations.Clear()

    member _.ScriptUpdated = scriptUpdatedEvent.Publish

    /// Get compilation defines and language version relevant for syntax processing.
    /// Quicker then TryGetOptionsForDocumentOrProject as it doesn't need to recompute the exact project
    /// options for a script.
    member this.GetCompilationDefinesAndLangVersionForEditingDocument(document: Document) =
        async {
            let! parsingOptions, _ = this.GetOptionsByProject(document.Project)

            return CompilerEnvironment.GetConditionalDefinesForEditing parsingOptions, parsingOptions.LangVersionText, parsingOptions.StrictIndentation
        }

    member _.Checker = checker
