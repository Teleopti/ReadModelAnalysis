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


let testFormatUsageChainsOfRmInWebClass =
    fun _ ->
        let rm = ReadModel "PersonScheduleDay"
        let snapshot = closureOfWc configValues [rm] []
        startingUsageOfRm rm snapshot.usages
        |> List.collect (fun u -> traceUsageChain u snapshot.usages)
        |> List.choose  formatUsageChainForWebClass 