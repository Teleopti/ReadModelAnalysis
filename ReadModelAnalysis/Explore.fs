module Explore

open Domain
open Config
open CodeDigest
open Discover

type Expansion<'T> = { targets : 'T list; usages : Usage list }

let _expand (targetCollection: 'T list) (seeds: 'U list) (discover: 'U -> 'T -> Usage option) : 'T Expansion =
    let needRecurse = typeof<'T> = typeof<'U>    
    let rec loop seeds' accTargets' accUsages'=
        let discoveredTargetUsages = 
            [
                for seed in seeds' do
                    for target in targetCollection do 
                        yield (seed, target)
            ]
            |> List.choose (
                function (seed, target) -> discover seed target |> Option.map (fun x -> (target, x)))
        let cast = List.toSeq >> Seq.cast >> Seq.toList
        let accUsages =  discoveredTargetUsages |> List.map snd |> List.append accUsages'  
        let accTargets = discoveredTargetUsages |> List.map fst |> List.append accTargets'      
        match discoveredTargetUsages, needRecurse with
        | [], _ -> accTargets, accUsages
        | _, false -> accTargets, accUsages
        | _, true -> loop (discoveredTargetUsages |> List.map fst |> cast) accTargets accUsages
    loop seeds [] [] |> function (ts', us') -> { targets = ts'; usages = us' }

let expandSpFromRm (configValues : ConfigValues) (seeds: ReadModel list) =
    let allSps = 
        scanStoredProcedureFiles configValues (
            fun spFile -> StoredProcedure ( spFile.getShortName(), spFile.Path) |> Some ) 
    _expand allSps seeds discoverRmUsedInSp

let expandSpFromSp (configValues : ConfigValues) (seeds: StoredProcedure list) =
    let allSps = 
        scanStoredProcedureFiles configValues (
            fun spFile -> StoredProcedure ( spFile.getShortName(), spFile.Path) |> Some ) 
    _expand allSps seeds discoverSpUsedInSp
    
        
    
    