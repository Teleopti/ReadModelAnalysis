open System
open IO
open Domain
open Config
open CodeDigest
open AssemblyDigest
open Discover
open Explore
open Present

let findReadModelUsageInWcJob readModel =
    let rm = ReadModel readModel
    let snapshot = closureOfWc configValues [rm] []
    presentInFile 
        (Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory) + @"\" + readModel + @".txt")
        (startingUsageOfRm rm snapshot.usages |> List.collect (fun u -> traceUsageChain u snapshot.usages))
        snapshot.usages

[<EntryPoint>]
let main argv = 
    argv
    |> Array.iter findReadModelUsageInWcJob
    printfn "%A done!" argv
    0 // return an integer exit code
