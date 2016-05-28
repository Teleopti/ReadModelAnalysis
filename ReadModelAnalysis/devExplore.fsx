#load "IO.fs"
#load "Domain.fs"
#load "Config.fs"
#load "CodeDigest.fs"
#load "AssemblyDigest.fs"
#load "Discover.fs"
#load "Explore.fs"

open IO
open Domain
open Config
open CodeDigest
open AssemblyDigest
open Discover
open Explore

let testInputOfIcs =
    fun _ ->
        let rm = ReadModel "PersonScheduleDay"
        inputOfIcs configValues [rm] |> getStateTargets

let testEquilibrateIcEhcDc =
    fun _ ->
        let rm = ReadModel "ScheduleProjectionReadOnly"
        let ics = inputOfIcs configValues [rm]  |> getStateTargets                
        let accIcs, accDcs, accEhcs, usages = equilibrateIcEhDc configValues ics [] [] []
        (accIcs, accDcs, accEhcs, usages)

let testEquilibrateIcEhcDc2 =
    fun _ ->
        let rm = ReadModel "PersonScheduleDay"
        let ics = inputOfIcs configValues [rm]  |> getStateTargets                
        let accIcs, accDcs, accEhcs, usages = equilibrateIcEhDc configValues ics [] [] []
        (accIcs, accDcs, accEhcs, usages)

let testClosureOfWc =
    fun _ ->
        let rm = ReadModel "PersonScheduleDay"
        closureOfWc configValues [rm] []

let testGetFirstUsagesOfRm =
    fun _ ->
        let rm = ReadModel "PersonScheduleDay"
        let snapshot = closureOfWc configValues [rm] []
        startingUsageOfRm rm snapshot.usages

let testTraceUsageChainsOfRm =
    fun _ ->
        let rm = ReadModel "PersonScheduleDay"
        let snapshot = closureOfWc configValues [rm] []
        startingUsageOfRm rm snapshot.usages
        |> List.collect (fun u -> traceUsageChain u snapshot.usages)
