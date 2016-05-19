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

let testClosureOfIcs = 
    fun _ ->
        let rm = ReadModel "ScheduleProjectionReadOnly"
        closureOfIcs configValues [rm] []

let testEquilibrateIcEhcDc =
    fun _ ->
        let rm = ReadModel "ScheduleProjectionReadOnly"
        let ics = inputOfIcs configValues [rm]  |> getStateTargets                
        let accIcs, accDcs, accEhcs, usages = equilibrateIcEhDc configValues ics [] [] []
        (accIcs, accDcs, accEhcs, usages)