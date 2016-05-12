module Explore

open Domain
open Config
open CodeDigest
open AssemblyDigest
open Discover

type Snapshot<'T> = { targets : 'T list; usages : Usage list }

let usageStateReturn x = fun usages -> { targets = x; usages = usages }
let usageStateBind f x =
    fun usages ->
        let { targets = targets'; usages = usages' } = x usages
        let { targets = targets''; usages = usages'' } = f targets' usages'
        {targets = targets''; usages = List.concat [usages'; usages'']}

type UsageState() =
    member this.Return(x) = usageStateReturn x
    member this.Bind(x, f) = usageStateBind f x
    member this.ReturnFrom(x) = x

let usageState = new UsageState()

let _explore (discover: 'U -> 'T -> Usage option) (seeds: 'U list) (targetCollection: 'T list) : Usage list -> 'T Snapshot =
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
        let accUsages =  discoveredTargetUsages |> List.map snd |> List.append accUsages'  |> List.distinct
        let accTargets = discoveredTargetUsages |> List.map fst |> List.append accTargets' |> List.distinct     
        match discoveredTargetUsages, needRecurse with
        | [], _ -> accTargets, accUsages
        | _, false -> accTargets, accUsages
        | _, true -> 
            let newSeeds = discoveredTargetUsages |> List.map fst |> cast |> List.where (fun x -> List.contains x seeds' |> not)
            loop newSeeds accTargets accUsages
    loop seeds [] [] |> function (ts', us') -> (fun usages -> { targets = ts'; usages = List.append usages us' })


let exploreSpFromRm configValues seeds  =
    _explore discoverRmUsedInSp seeds <| getAllStoredProcedures configValues

let exploreSpFromSp configValues seeds =
    _explore discoverSpUsedInSp seeds <| getAllStoredProcedures configValues

let closureOfSps configValues seeds =
    usageState {
        let! sps' = exploreSpFromRm configValues seeds
        let! sps'' = exploreSpFromSp configValues sps'
        return List.append sps' sps''
    }

let exploreNqFromRm configValues seeds =
    _explore discoverRmUsedInNq seeds <| getAllNhibQueries configValues

let closureOfNqs configValues seeds =
    exploreNqFromRm configValues seeds

let exploreIcFromSp configValues seeds =
    _explore discoverSpUsedInIc seeds <| getAllInfraClasses configValues

let exploreIcFromRm configValues seeds =
    _explore discoverRmUsedInIc seeds <| getAllInfraClasses configValues

let exploreIcFromNq configValues seeds =
    _explore discoverNqUsedInIc seeds <| getAllInfraClasses configValues

let exploreIcFromIc configValues seeds =
    _explore discoverIcUsedInIc seeds <| getAllInfraClasses configValues

let exploreEhcFromRm configValues seeds = 
    _explore discoverRmUsedInEh seeds <| getAllEventHandlerClasses configValues

let exploreEhcFromSp configValues seeds =
    _explore discoverSpUsedInEh seeds <| getAllEventHandlerClasses configValues
    

let exploreEhcFromNq configValues seeds =
    _explore discoverNqUsedInEh seeds <| getAllEventHandlerClasses configValues

let exploreIcFromEhc configValues seeds =
    _explore discoverEhcHandlesIc seeds <| getAllInfraClasses configValues

let closureOfIcs (configValues : ConfigValues) (seeds: ReadModel list) =
    usageState {
        let! sps = closureOfSps configValues seeds
        let! nqs = closureOfNqs configValues seeds
        let! eh1 = exploreEhcFromRm configValues seeds
        let! eh2 = exploreEhcFromSp configValues sps
        let! eh3 = exploreEhcFromNq configValues nqs
        let! ic1 = exploreIcFromRm configValues seeds
        let! ic2 = exploreIcFromSp configValues sps
        let! ic3 = exploreIcFromNq configValues nqs
        let! ic4 = exploreIcFromEhc configValues <| List.concat [eh1; eh2; eh3]
        let ic' = List.concat [ic1; ic2; ic3; ic4]
        let! ic5 = exploreIcFromIc configValues ic'
        return List.append ic' ic5
    }          
