namespace FSharp.Test

open FSharp.Compiler.IO
open System.Threading
open System.Collections.Concurrent
open System
open System.IO
open System.IO.MemoryMappedFiles
open System.Reflection

type TestFileSystem() =
    inherit DefaultFileSystem()

    static let virtuals = AsyncLocal<_>()

    do virtuals.Value <- ConcurrentDictionary<string, MemoryStream>()

    let tryGetVirtual(filePath: string) =
        match virtuals.Value.TryGetValue(filePath) with
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
        virtuals.Value.GetOrAdd(filePath, fun _ -> new MemoryStream())

    override _.FileExistsShim (fileName: string): bool =
        match virtuals.Value.TryGetValue(fileName) with
        | true, _ -> true
        | _ -> base.FileExistsShim(fileName)

    override _.FileDeleteShim (fileName: string): unit =
        virtuals.Value.TryRemove(fileName) |> ignore

    override this.CopyShim (src: string, dest: string, overwrite: bool) =
        ignore overwrite
        use srcStream: Stream =
            match virtuals.Value.TryGetValue(src) with
            | true, mms -> new MemoryStream(mms.GetBuffer(), 0, int mms.Length, writable = false)
            | _ -> base.OpenFileForReadShim(src)
        use destStream = this.OpenFileForWriteShim(dest)
        destStream.Position <- 0
        srcStream.CopyTo(destStream)

    member _.Materialize(filePath: string) =
        let tempPath = Path.GetTempPath()
        match virtuals.Value.TryGetValue(filePath) with
        | true, mms ->
            let dest = File.OpenWrite(tempPath)
            mms.Position <- 0
            mms.CopyTo dest
            tempPath
        | _ -> raise (IOException($"TestFileSystem does not contain {filePath}"))

    member _.TryGetVirtual(filePath: string) = tryGetVirtual filePath

    override fileSystem.AssemblyLoader =
        {
            new IAssemblyLoader with
                member _.AssemblyLoad (assemblyName: AssemblyName): Assembly = 
                    raise (NotImplementedException())

                member _.AssemblyLoadFrom (fileName: string): Assembly =
                    match fileSystem.TryGetVirtual(fileName) with
                    | Some mms ->
                        try
                            Assembly.Load(mms.ToArray())
                        with
                        | _ ->
                            Assembly.LoadFrom(fileSystem.Materialize(fileName))
                    | _ ->
                         Assembly.LoadFrom(fileSystem.Materialize(fileName))
        }
        