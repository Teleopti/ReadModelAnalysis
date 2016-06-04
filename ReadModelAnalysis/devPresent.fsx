#load "IO.fs"
#load "Domain.fs"
#load "Config.fs"
#load "CodeDigest.fs"
#load "AssemblyDigest.fs"
#load "Discover.fs"
#load "Explore.fs"
#load "Present.fs"

open IO
open Domain
open Config
open CodeDigest
open AssemblyDigest
open Discover
open Explore
open Present
open System

let testFormatUsageChainsOfRmInWebClass =
    fun _ ->
        let rm = ReadModel "PersonScheduleDay"
        let snapshot = closureOfWc configValues [rm] []
        startingUsageOfRm rm snapshot.usages
        |> List.collect (fun u -> traceUsageChain u snapshot.usages)
        |> List.choose  formatUsageChainForWebClass 


let testWriteToFile =
    fun _ ->
        let rm = ReadModel "PersonScheduleDay"
        let snapshot = closureOfWc configValues [rm] []   
        use writer = new System.IO.StreamWriter(Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory) + @"\testWriteToFile.txt", append = false)            
        startingUsageOfRm rm snapshot.usages
        |> List.collect (fun u -> traceUsageChain u snapshot.usages)
        |> List.choose  formatUsageChainForWebClass  
        |> List.iter (fprintfn writer "%s")
        fprintfn writer "\r\n\r\n---------------------------------\r\n\r\n"
        snapshot.usages
        |> List.iter (fprintfn writer "%90A")       
     