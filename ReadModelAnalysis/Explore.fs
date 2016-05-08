module Explore

open Domain
open Config
open CodeDigest
open Discover


let _expand (targetCollection: 'T list) (seeds: 'U list) (discover: 'U -> 'T -> Usage option) =
    let needRecurse = typeof<'T> = typeof<'U>    
    let rec loop seeds' usages' =
        let discoveredTargetUsages = 
            [
                for seed in seeds' do
                    for target in targetCollection do 
                        yield (seed, target)
            ]
            |> List.choose (
                function (seed, target) -> discover seed target |> Option.map (fun x -> (target, x)))
        let cast = List.toSeq >> Seq.cast >> Seq.toList
        let usages =  discoveredTargetUsages |> List.map snd |> List.append usages'        
        match discoveredTargetUsages, needRecurse with
        | [], _ -> usages
        | _, false -> usages
        | _, true -> loop (discoveredTargetUsages |> List.map fst |> cast) usages
    loop seeds []

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
    
        
    
    