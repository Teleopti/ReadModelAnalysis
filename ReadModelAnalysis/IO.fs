module IO

open System.IO

type File' private ( path: string, fileInfo: FileInfo) =
    static member toFile' (path: string) =
        File'(path = path, fileInfo = new FileInfo(path))
    static member toFile' (fileInfo: FileInfo) =
        File'(path = fileInfo.FullName, fileInfo = fileInfo)
    member val Path = path
    member val FileInfo = fileInfo
    member this.getLines () : string list = 
        File.ReadAllLines(this.Path) |> Array.toList
    member this.getShortName () : string =
        fileInfo.Name.Replace(fileInfo.Extension, "")

type Dir' private ( path: string, dirInfo: DirectoryInfo) = 
    static member toDir' (path: string) =
        Dir'(path = path, dirInfo = new DirectoryInfo(path))
    static member toDir' (dirInfo: DirectoryInfo) =
        Dir'(path = dirInfo.FullName, dirInfo = dirInfo)
    member val Path = path
    member val DirInfo = dirInfo
    member this.getFiles (filter: File' -> bool) =
        [ 
            yield! (this.DirInfo.GetFiles() |> Seq.map File'.toFile' |> Seq.filter filter)
            for subdir in this.DirInfo.GetDirectories() do
                yield! (Dir'.toDir' subdir).getFiles(filter)]
    member this.getFiles () = this.getFiles (fun _ -> true)
