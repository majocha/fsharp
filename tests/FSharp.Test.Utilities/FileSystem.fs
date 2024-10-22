namespace FSharp.Test

open FSharp.Compiler.IO
open System.Threading
open System.Collections.Concurrent
open System
open System.IO
open System.IO.MemoryMappedFiles
open System.Reflection

module TestFileSystem =   
    let virtuals = AsyncLocal<_>()


type TestFileSystem(?initial) =
    inherit DefaultFileSystem()

    do TestFileSystem.virtuals.Value <- defaultArg initial (ConcurrentDictionary<string, MemoryStream>())

    let tryGetVirtual(filePath: string) =
        match TestFileSystem.virtuals.Value.TryGetValue(filePath) with
        | true, mms -> Some mms
        | _ -> None

    override _.OpenFileForReadShim(filePath: string, ?useMemoryMappedFile, ?shouldShadowCopy) =
        match tryGetVirtual filePath with 
        | Some mms -> new MemoryStream(mms.GetBuffer())
        | _ -> base.OpenFileForReadShim(filePath, ?useMemoryMappedFile = useMemoryMappedFile, ?shouldShadowCopy = shouldShadowCopy)

    override _.OpenFileForWriteShim(filePath: string, ?fileMode: FileMode, ?fileAccess: FileAccess, ?fileShare: FileShare) =
        ignore fileMode
        ignore fileAccess
        ignore fileShare
        TestFileSystem.virtuals.Value.AddOrUpdate(filePath, (fun _ -> new MemoryStream()), (fun _ ms -> new MemoryStream(ms.GetBuffer())))

    override _.FileExistsShim (fileName: string): bool =
        tryGetVirtual fileName |> Option.isSome || base.FileExistsShim(fileName)

    override _.FileDeleteShim (fileName: string): unit =
        TestFileSystem.virtuals.Value.TryRemove(fileName) |> ignore

    override this.CopyShim (src: string, dest: string, overwrite: bool) =
        ignore overwrite
        use srcStream = this.OpenFileForReadShim src
        use destStream = this.OpenFileForWriteShim dest
        srcStream.CopyTo(destStream)

    member _.Materialize(filePath: string) =
        let tempPath = Path.GetTempPath()
        match tryGetVirtual filePath with
        | Some ms ->
            let physical = File.OpenWrite(tempPath)
            use ms =  new MemoryStream(ms.GetBuffer())
            ms.CopyTo physical
            tempPath
        | _ -> raise (IOException($"TestFileSystem does not contain {filePath}"))

    member _.TryGetVirtual(filePath: string) = tryGetVirtual filePath

    override fileSystem.AssemblyLoader =
        {
            new IAssemblyLoader with
                member _.AssemblyLoad (assemblyName: AssemblyName): Assembly =
                    Assembly.Load assemblyName

                member _.AssemblyLoadFrom (fileName: string): Assembly =
                    match fileSystem.TryGetVirtual(fileName) with
                    | Some ms ->
                        try
                            Assembly.Load(ms.ToArray())
                        with
                        | _ ->
                            Assembly.LoadFrom(fileSystem.Materialize(fileName))
                    | _ ->
                         Assembly.LoadFrom(fileSystem.Materialize(fileName))
        }
        