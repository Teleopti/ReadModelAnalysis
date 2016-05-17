#load "IO.fs"
#load "Domain.fs"
#load "Config.fs"
#load "CodeDigest.fs"
#load "AssemblyDigest.fs"
#load "Discover.fs"

open IO
open Domain
open Config
open CodeDigest
open AssemblyDigest
open Discover

let testDiscoverNqUsedInDc =
    fun () ->
        let dcs = getAllDomainClasses configValues
        let nqs = getAllNhibQueries configValues      
        [
            for dc in dcs do
                for nq in nqs do
                    yield (dc, nq)
        ]
        |> List.choose (fun (dc, nq) -> discoverNqUsedInDc nq dc)

let testDiscoverNqUsedInIc =
    fun () ->
        let cs = getAllInfraClasses configValues
        let nqs = getAllNhibQueries configValues      
        [
            for c in cs do
                for nq in nqs do
                    yield (c, nq)
        ]
        |> List.choose (fun (c, nq) -> discoverNqUsedInIc nq c)

let testDiscoverSpUsedInDc =
    fun () ->
        let dcs = getAllDomainClasses configValues
        let sps = getAllStoredProcedures configValues      
        [
            for dc in dcs do
                for sp in sps do
                    yield (dc, sp)
        ]
        |> List.choose (fun (dc, sp) -> discoverSpUsedInDc sp dc)

let testDiscoverRmUsedInEhc =
    fun () ->
        let cs = getAllEventHandlerClasses configValues
        let rms = getAllReadModels   
        [
            for c in cs do
                for rm in rms do
                    yield (c, rm)
        ]
        |> List.choose (fun (c, rm) -> discoverRmUsedInEh rm c)

let testDiscoverSpUsedInEhc =
    fun () ->
        let cs = getAllEventHandlerClasses configValues
        let sps = getAllStoredProcedures configValues      
        [
            for c in cs do
                for sp in sps do
                    yield (c, sp)
        ]
        |> List.choose (fun (c, sp) -> discoverSpUsedInEh sp c)

let testDiscoverNqUsedInEhc =
    fun () ->
        let cs = getAllEventHandlerClasses configValues
        let qs = getAllNhibQueries configValues   
        [
            for c in cs do
                for q in qs do
                    yield (c, q)
        ]
        |> List.choose (fun (c, q) -> discoverNqUsedInEh q c)

let testDiscoverIcUsedInDc =
    fun () ->
        let ics = getAllInfraClasses configValues
        let dcs = getAllDomainClasses configValues
        
           
        let mutable progress = 0
        [
            for i, ic in ics |> List.indexed do
                for dc in dcs do
                    yield (i, ic, dc)
        ]     
        |> List.choose (
            fun (i, ic, dc) ->
                if progress <> i 
                then 
                    progress <- i
                    printfn "-- %d" progress
                discoverIcUsedInDc ic dc)

let testDiscoverIcUsedInDc2 =
    fun () ->
        let ic = InfraClass ("HangfireAsSyncEventPublisher", @"C:\Teleopti\Infrastructure\ApplicationLayer\HangfireAsSyncEventPublisher.cs")
        let dc = DomainClass ("IntradayOptimizationCommandHandler", @"C:\Teleopti\Domain\ApplicationLayer\ResourcePlanner\IntradayOptimizationCommandHandler.cs")
        let ics = [ic]
        let dcs = [dc]   
        [
            for ic in ics do
                for dc in dcs do
                    yield (ic, dc)
        ]
        |> List.choose (fun (ic, dc) -> discoverIcUsedInDc ic dc)

let testDiscoverIcUsedInDc3 =
    fun () ->
        let ic = InfraClass ("ScheduleDictionaryPersister", @"C:\Teleopti\Infrastructure\Persisters\Schedules\ScheduleDictionaryPersister.cs")
        let dcs = getAllDomainClasses configValues        
        dcs
        |> List.choose (
            fun dc ->
                discoverIcUsedInDc ic dc
        )       

let testDiscoverIcUsedInDc4 =
    fun () ->
        let ic = InfraClass ("ScheduleDictionaryPersister", @"C:\Teleopti\Infrastructure\Persisters\Schedules\ScheduleDictionaryPersister.cs")
        let dc = DomainClass ("ScheduleOptimization", @"C:\Teleopti\Domain\Optimization\ScheduleOptimization.cs")
        let ics = [ic]
        let dcs = [dc]   
        [
            for ic in ics do
                for dc in dcs do
                    yield (ic, dc)
        ]
        |> List.choose (fun (ic, dc) -> discoverIcUsedInDc ic dc)

let testDiscoverDcUsedInIc =
    fun () ->
        let ics = getAllInfraClasses configValues
        let dcs = getAllDomainClasses configValues
        let mutable progress = 0
        [
            for i, dc in dcs |> List.indexed do
                for ic in ics do
                    yield (i, ic, dc)
        ]     
        |> Seq.choose (
            fun (i, ic, dc) ->
                if progress <> i 
                then 
                    progress <- i
                    printfn "-- %d" progress
                discoverDcUsedInIc dc ic)
        |> Seq.take 10