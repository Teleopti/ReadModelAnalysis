module Explore

open Domain
open Config
open CodeDigest
open AssemblyDigest
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
    let allTargets = 
        scanStoredProcedureFiles configValues (
            fun afile -> StoredProcedure ( afile.getShortName(), afile.Path) |> Some ) 
    _expand allTargets seeds discoverRmUsedInSp

let expandSpFromSp (configValues : ConfigValues) (seeds: StoredProcedure list) =
    let allTargets = 
        scanStoredProcedureFiles configValues (
            fun spFile -> StoredProcedure ( spFile.getShortName(), spFile.Path) |> Some ) 
    _expand allTargets seeds discoverSpUsedInSp

let closureOfSps (configValues : ConfigValues) (seeds: ReadModel list) =
    let { targets = targets' ; usages = usages' } = expandSpFromRm configValues seeds
    let { targets = targets''; usages = usages''} = expandSpFromSp configValues targets'
    { targets = List.concat [targets'; targets'']; usages = List.append usages' usages''}   

let expandNqFromRm (configValues : ConfigValues) (seeds: ReadModel list) =
    let allTargets = 
        scanNhibMappingFiles configValues (discoverSqlQueryInNhibMapping >> Some) |> List.concat
    _expand allTargets seeds discoverRmUsedInNq

let closureOfNqs (configValues : ConfigValues) (seeds: ReadModel list) =
    let { targets = targets' ; usages = usages' } = expandNqFromRm configValues seeds   
    { targets = targets'; usages = usages'}   

let expandIcFromSp (configValues : ConfigValues) (seeds: StoredProcedure list) =
    let allTargets = 
        scanInfraClassFiles configValues (discoverClassInFile InfraClass >> Some) |> List.concat             
    _expand allTargets seeds discoverSpUsedInIc

let expandIcFromRm (configValues : ConfigValues) (seeds: ReadModel list) =
    let allTargets = 
        scanInfraClassFiles configValues (discoverClassInFile InfraClass >> Some) |> List.concat             
    _expand allTargets seeds discoverRmUsedInIc

let expandIcFromNq (configValues : ConfigValues) (seeds: NhibQuery list) =
    let allTargets = 
        scanInfraClassFiles configValues (discoverClassInFile InfraClass >> Some) |> List.concat             
    _expand allTargets seeds discoverNqUsedInIc

let expandIcFromIc (configValues : ConfigValues) (seeds: InfraClass list) =
    let allTargets = 
        scanInfraClassFiles configValues (discoverClassInFile InfraClass >> Some) |> List.concat             
    _expand allTargets seeds discoverIcUsedInIc

let expandEhcFromRm (configValues : ConfigValues) (seeds: ReadModel list) =                   
    let allTargets = getAllEventHandlerClasses configValues            
    _expand allTargets seeds discoverRmUsedInEh

let expandEhcFromSp (configValues : ConfigValues) (seeds: StoredProcedure list) =                   
    let allTargets = getAllEventHandlerClasses configValues            
    _expand allTargets seeds discoverSpUsedInEh

let expandEhcFromNq (configValues : ConfigValues) (seeds: NhibQuery list) =                   
    let allTargets = getAllEventHandlerClasses configValues            
    _expand allTargets seeds discoverNqUsedInEh

let expandIcFromEhc (configValues: ConfigValues) (seeds: EventHandlerClass list) =
    let allTargets = 
        scanInfraClassFiles configValues (discoverClassInFile InfraClass >> Some) |> List.concat             
    _expand allTargets seeds discoverEhcHandlesIc

let closureOfIcs (configValues : ConfigValues) (seeds: ReadModel list) =
    let { targets = sps ; usages = spUsages } = closureOfSps configValues seeds
    let { targets = nqs; usages = nqUsages} = closureOfNqs configValues seeds
    let { targets = targets' ; usages = usages' } = expandIcFromRm configValues seeds
    let { targets = targets''; usages = usages''} = expandIcFromSp configValues sps
    let { targets = targets'''; usages = usages'''} = expandIcFromNq configValues nqs
    let ics = List.concat [targets'; targets''; targets''']
    let { targets = targets''''; usages = usages''''} = expandIcFromIc configValues ics
    { targets = List.concat [ics; targets'''']; usages = List.concat [usages'; usages''; usages'''; usages'''']}   
