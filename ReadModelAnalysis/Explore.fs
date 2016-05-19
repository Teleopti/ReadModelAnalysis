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
        {targets = targets''; usages = List.concat [usages; usages'; usages''] |> List.distinct}
let getStateTargets (s: Usage list -> Snapshot<'T>) = s [] |> fun x -> x.targets   

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
    loop seeds [] [] 
    |> function (ts', us') -> (fun usages -> { targets = ts'; usages = us' })


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
    printfn "@@ exploreIcFromIc %A " seeds
    _explore discoverIcUsedInIc seeds <| getAllInfraClasses configValues

let exploreIcFromDc configValues seeds =
    printfn "@@ exploreIcFromDc %A " seeds
    _explore discoverDcUsedInIc seeds <| getAllInfraClasses configValues

let exploreDcFromSp configValues seeds =
    _explore discoverSpUsedInDc seeds <| getAllDomainClasses configValues

let exploreDcFromNq configValues seeds =
    _explore discoverNqUsedInDc seeds <| getAllDomainClasses configValues

let exploreDcFromIc configValues seeds =
    printfn "@@ exploreDcFromIc %A " seeds
    _explore discoverIcUsedInDc seeds <| getAllDomainClasses configValues

let exploreDcFromDc configValues seeds =
    printfn "@@ exploreDcFromDc %A " seeds
    _explore discoverDcUsedInDc seeds <| getAllDomainClasses configValues


let exploreEhcFromRm configValues seeds = 
    _explore discoverRmUsedInEhc seeds <| getAllEventHandlerClasses configValues

let exploreEhcFromSp configValues seeds =
    _explore discoverSpUsedInEhc seeds <| getAllEventHandlerClasses configValues

let exploreEhcFromNq configValues seeds =
    _explore discoverNqUsedInEhc seeds <| getAllEventHandlerClasses configValues

let exploreIcFromEhc configValues seeds =
    printfn "@@ exploreIcFromEhc %A " seeds
    _explore discoverEhcHandlesIc seeds <| getAllInfraClasses configValues

let exploreDcFromEhc configValues seeds =
    printfn "@@ exploreDcFromEhc %A " seeds
    _explore discoverEhcHandlesDc seeds <| getAllDomainClasses configValues

let exploreEhcFromIc configValues seeds =
    printfn "@@ exploreEhcFromIc %A " seeds
    _explore discoverIcUsedInEhc seeds <| getAllEventHandlerClasses configValues

let exploreEhcFromDc configValues seeds =
    printfn "@@ exploreEhcFromDc %A " seeds
    _explore discoverDcUsedInEhc seeds <| getAllEventHandlerClasses configValues

let exploreEhcFromEhc configValues seeds =
    printfn "@@ exploreEhcFromEhc %A " seeds
    _explore discoverEhcHandlesEhc seeds <| getAllEventHandlerClasses configValues

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
        let ic' = List.concat [ic1; ic2; ic3; ic4] |> List.distinct
        let! ic5 = exploreIcFromIc configValues ic'
        return List.append ic' ic5
    }          

let inputOfIcs configValues (seeds: ReadModel list) = 
    usageState {
        let! sps = closureOfSps configValues seeds
        let! nqs = closureOfNqs configValues seeds        
        let! ic1 = exploreIcFromRm configValues seeds
        let! ic2 = exploreIcFromSp configValues sps
        let! ic3 = exploreIcFromNq configValues nqs
        return List.concat [ic1; ic2; ic3 ] |> List.distinct        
    } 

let equilibrateIcEhDc configValues ics dcs ehcs =
    let merge3 = fun (x, y, z) -> List.concat [x; y; z] |> List.distinct 
    let diff = fun (x, y, z, o) -> merge3 (x,y,z) |> List.where (fun e -> List.contains e o |> not)

    let rec loop (seedIcs, accIcs) (seedDcs, accDcs) (seedEhcs, accEhcs) usages =
        printfn "----- loop usages %A" usages
        let deltaIcs =
            usageState {
                let! ics1 = exploreIcFromIc configValues seedIcs
                let! ics2 = exploreIcFromDc configValues seedDcs
                let! ics3 = exploreIcFromEhc configValues seedEhcs
                return diff(ics1, ics2, ics3, accIcs)               
            }         
        let deltaDcs =
            usageState {
                let! dcs1 = exploreDcFromIc configValues seedIcs
                let! dcs2 = exploreDcFromDc configValues seedDcs
                let! dcs3 = exploreDcFromEhc configValues seedEhcs
                let oo = diff(dcs1, dcs2, dcs3, accDcs)   
                return oo
            }          
        let deltaEhcs =
            usageState {
                let! ehcs1 = exploreEhcFromIc configValues seedIcs
                let! ehcs2 = exploreEhcFromDc configValues seedDcs
                let! ehcs3 = exploreEhcFromEhc configValues seedEhcs
                return diff(ehcs1, ehcs2, ehcs3, accEhcs)         
            }
        let icSnapshot = deltaIcs usages
        let dcSnapshot = deltaDcs icSnapshot.usages
        let ehcSnapshot = deltaEhcs dcSnapshot.usages

        let hasDelta = (List.length icSnapshot.targets + List.length dcSnapshot.targets + List.length ehcSnapshot.targets) <> 0
        if hasDelta 
        then            
            loop (icSnapshot.targets, List.append accIcs icSnapshot.targets)
                 (dcSnapshot.targets, List.append accDcs dcSnapshot.targets)
                 (ehcSnapshot.targets, List.append accEhcs ehcSnapshot.targets)
                 ehcSnapshot.usages
        else
            accIcs, accDcs, accEhcs, usages
    loop (ics, ics) (dcs, dcs) (ehcs, ehcs)