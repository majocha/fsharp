namespace UseLocalCompiler.FSharp.Build.Tasks

open System
open System.IO
type public Fsc() =
    inherit FSharp.Build.Fsc()

    static member private TryGetBuildContext() =
        try
            let assemblyDir = DirectoryInfo(Path.GetDirectoryName(typeof<Fsc>.Assembly.Location))

            if
                not (isNull assemblyDir.Parent)
                && not (isNull assemblyDir.Parent.Parent)
                && not (isNull assemblyDir.Parent.Parent.Parent)
                && not (isNull assemblyDir.Parent.Parent.Parent.Parent)
                && not (isNull assemblyDir.Parent.Parent.Parent.Parent.Parent)
                && StringComparer.OrdinalIgnoreCase.Equals(assemblyDir.Parent.Parent.Parent.Name, "bin")
                && StringComparer.OrdinalIgnoreCase.Equals(assemblyDir.Parent.Parent.Parent.Parent.Name, "artifacts")
            then
                Some(assemblyDir.Parent.Parent.Parent.Parent.Parent.FullName, assemblyDir.Parent.Name)
            else
                None
        with _ ->
            None

    static member private TryGetLocalDotnetFscCompilerPath(repoRoot: string, configuration: string) =
        let fscOutputRoot = Path.Combine(repoRoot, "artifacts", "bin", "fsc", configuration)

        if Directory.Exists fscOutputRoot then
            fscOutputRoot
            |> Directory.GetDirectories
            |> Array.choose (fun dir ->
                let candidate = Path.Combine(dir, "fsc.dll")

                if File.Exists candidate then
                    Some candidate
                else
                    None)
            |> function
                | [| candidate |] -> Some candidate
                | _ -> None
        else
            None

    override _.GenerateCommandLineCommands() =
        match Fsc.TryGetBuildContext() with
        | Some(repoRoot, configuration) ->
            match Fsc.TryGetLocalDotnetFscCompilerPath(repoRoot, configuration) with
            | Some localDotnetFscCompilerPath -> localDotnetFscCompilerPath
            | None -> base.GenerateCommandLineCommands()
        | None -> base.GenerateCommandLineCommands()
