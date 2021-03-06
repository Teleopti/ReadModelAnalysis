﻿#load "IO.fs"
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

let rm1 = ReadModel "rm1"
let rm2 = ReadModel "rm2"
let sp1 = StoredProcedure ("sp1", "/sp1")
let sp2 = StoredProcedure ("sp2", "/sp2")
let dc1 = DomainClass ("dc1", "/dc1")
let wc1 = WebClass ("wc1", "/wc1")

let usage1 = RmUsedInSp { target = rm1; host = sp1; locs = [{ hostLoc = "sp1loc1"; targetLocs = ["rm1loc1"] }]}
let usage2 = SpUsedInDc { target = sp1; host = dc1; locs = [{ hostLoc = "dc1loc1"; targetLocs = ["sp1loc1"] }]}
let usage3 = DcUsedInWc { target = dc1; host = wc1; locs = [{ hostLoc = "wc1loc1"; targetLocs = ["dc1loc1"] }]}

let testTraceUsageChain = 
    fun _ -> 
        traceUsageChain usage1 [ usage1; usage2; usage3 ];;

let testDiscoverReadModelUsedInNhibMapping =
    fun _ ->
        let allNhibQueries = 
            scanNhibMappingFiles configValues (discoverSqlQueryInNhibMapping >> Some) |> List.concat       
        let rm = ReadModel "GroupingReadOnly" 
        allNhibQueries
        |>  List.choose (discoverRmUsedInNq rm)        

let testDiscoverReadModelUsedInStoredProcedure =
    fun _ ->
        let present (file : File') = 
           let sp = StoredProcedure (file.getShortName(), file.Path)
           let rm = ReadModel "GroupingReadOnly"
           discoverRmUsedInSp rm sp  
        scanStoredProcedureFiles configValues present

let testDiscoverStoredProcedureUsedInStoredProcedure =
    fun _ ->
        let present (targetSp : StoredProcedure) (file : File') = 
           let sp = StoredProcedure (file.getShortName(), file.Path) 
           if (sp = targetSp)
           then None
           else discoverSpUsedInSp targetSp sp                     
        let allSps = 
            scanStoredProcedureFiles configValues (
                fun spFile -> StoredProcedure ( spFile.getShortName(), spFile.Path) |> Some )
        [
            for sp in allSps do
                yield! scanStoredProcedureFiles configValues (present sp)
        ]

let testDiscoverClassInFile =
    fun _ ->
        let path = @"C:\Teleopti\Domain\ApplicationLayer\AbsenceRequests\NewAbsenceReportEventHandler.cs"
        let file = File'.toFile'(path)     
        discoverClassInFile id file
       
let testDiscoverRmUsedInIc =
    fun _ ->
        let allIcs =
            scanInfraClassFiles configValues  (discoverClassInFile InfraClass >> Some) |> List.concat 
        let rm = ReadModel "PersonScheduleDay"                               
        allIcs 
        |> List.choose (discoverRmUsedInIc rm) 

let testDiscoverSpUsedInDc =
    fun _ ->
        let allDcs =
            scanDomainClassFiles configValues  (discoverClassInFile DomainClass >> Some) |> List.concat 
        let allSps = 
            scanStoredProcedureFiles configValues (
                fun spFile -> StoredProcedure ( spFile.getShortName(), spFile.Path) |> Some )                
        [ for dc in allDcs do for sp in allSps do yield (sp, dc) ]
        |> List.choose (fun (sp, dc) -> discoverSpUsedInDc sp dc)  
        
let testDiscoverSpUsedInIc =
    fun _ ->
        let allIcs =
            scanInfraClassFiles configValues  (discoverClassInFile InfraClass >> Some) |> List.concat 
        let allSps = 
            scanStoredProcedureFiles configValues (
                fun spFile -> StoredProcedure ( spFile.getShortName(), spFile.Path) |> Some )                
        [ for ic in allIcs do for sp in allSps do yield (sp, ic) ]
        |> List.choose (fun (sp, ic) -> discoverSpUsedInIc sp ic)           
                    
let testDiscoverNqUsedInDc =
    fun _ ->
        let allDcs =
            scanDomainClassFiles configValues  (discoverClassInFile DomainClass >> Some) |> List.concat 
        let allNhibQueries = 
            scanNhibMappingFiles configValues (discoverSqlQueryInNhibMapping >> Some) |> List.concat                 
        [ for dc in allDcs do for nq in allNhibQueries do yield (nq, dc) ]
        |> List.choose (fun (nq, dc) -> discoverNqUsedInDc nq dc)         

let testNhibQueryPattern =
    fun _ ->
        let target = """ var uniqueSchedulePeriods = Session.GetNamedQuery("UniqueSchedulePeriods")   """
        match target with 
        | NhibQueryPattern "UniqueSchedulePeriods" _ -> printfn "Matched"
        | _ -> printfn "Not matched"                          

let testDiscoverNqUsedInIc =
    fun _ ->
        let allIcs =
            scanInfraClassFiles configValues  (discoverClassInFile InfraClass >> Some) |> List.concat 
        let allNhibQueries = 
            scanNhibMappingFiles configValues (discoverSqlQueryInNhibMapping >> Some) |> List.concat                 
        [ for ic in allIcs do for nq in allNhibQueries do yield (nq, ic) ]
        |> List.choose (fun (nq, ic) -> discoverNqUsedInIc nq ic)  

let testDiscoverIcUsedInDc =
    fun _ ->
        let ic = InfraClass ("PersonAssignmentRepository", @"C:\Teleopti\Infrastructure\Repositories\PersonAssignmentRepository.cs")
        let allDcs =
            scanDomainClassFiles configValues  (discoverClassInFile DomainClass >> Some) |> List.concat 
        allDcs
        |> List.choose (fun dc -> discoverIcUsedInDc ic dc)

let testDiscoverIcUsedInIc =
    fun _ ->
        let ic = InfraClass ("AbsenceRequestUpdater", @"C:\Teleopti\Infrastructure\Absence\AbsenceRequestUpdater.cs")
        let allIcs =
            scanInfraClassFiles configValues  (discoverClassInFile InfraClass >> Some) |> List.concat 
        allIcs
        |> List.choose (fun dc -> discoverIcUsedInIc ic dc)

let testDiscoverDcUsedInDc =
    fun _ ->
        let dc = DomainClass ("CreateOrUpdateSkillDays", @"C:\Teleopti\Domain\Outbound\CreateOrUpdateSkillDays.cs")
        let allDcs =
            scanDomainClassFiles configValues  (discoverClassInFile DomainClass >> Some) |> List.concat 
        allDcs
        |> List.choose (fun dc' -> discoverDcUsedInDc dc dc')

let testListAllEventHandlerClasses =
    fun _ ->
        let present t = 
            checkEventHandlerType t 
            |> Option.map (                
                fun es ->
                    let events = es |> List.map EventClass 
                    EventHandlerClass (t.Name, domainClassTypeToPath configValues t, events))
        scanDomainClasses configValues present

let testDiscoverEhcHandlesDc =
    fun _ ->
        let dc = DomainClass ("PersonAssignment", @"C:\Teleopti\Domain\Scheduling\Assignment\PersonAssignment.cs")
        let present t = 
            checkEventHandlerType t 
            |> Option.bind (                
                fun es ->
                    let events = es |> List.map EventClass 
                    let ehc = EventHandlerClass (t.Name, domainClassTypeToPath configValues t, events)
                    discoverEhcHandlesDc ehc dc)
        scanDomainClasses configValues present

let testDiscoverEhcHandlesEhc =
    fun _ ->
        let getEventHandlerType t =
            checkEventHandlerType t 
            |> Option.bind (                
                fun es ->
                    let events = es |> List.map EventClass 
                    EventHandlerClass (t.Name, domainClassTypeToPath configValues t, events) |> Some)                   
        let allHandlers = scanDomainClasses configValues getEventHandlerType
        [
            for handlerTarget in allHandlers do
                for handlerHost in allHandlers do
                    yield (handlerTarget, handlerHost)
        ]
        |> List.choose (function (target, host) -> discoverEhcHandlesEhc target host)

let testTraceNextUsageForEventHandlingUsage =
    fun _ ->
        let dc = DomainClass ("PersonAssignment", @"C:\Teleopti\Domain\Scheduling\Assignment\PersonAssignment.cs")
        let ehc1 = EventHandlerClass ("ScheduleChangedEventPublisher", @"C:\Teleopti\Domain\ApplicationLayer\ScheduleChangedEventHandlers\ScheduleChangedEventPublisher.cs", [EventClass "FullDayAbsenceAddedEvent"; EventClass "PersonAbsenceRemovedEvent"; EventClass "PersonAbsenceAddedEvent"; EventClass "ActivityAddedEvent"; EventClass "ActivityMovedEvent"; EventClass "PersonAbsenceModifiedEvent"; EventClass "DayOffAddedEvent"; EventClass "DayUnscheduledEvent"; EventClass "PersonAssignmentLayerRemovedEvent"])
        let ehc2 = EventHandlerClass("ScheduleChangedNotifier", "C:\Teleopti\Domain\ApplicationLayer\ScheduleChangedEventHandlers\ScheduleChangedNotifier.cs", [EventClass "ScheduleChangedEvent"])
        let usage1 = discoverEhcHandlesDc ehc1 dc |> Option.get
        let usage2 = discoverEhcHandlesEhc ehc2 ehc1 |> Option.get
        traceUsageChain usage2 [ usage1; usage2; ];;

let testExpandSpFromRm =
    fun _ ->
        let rm = ReadModel "FindPerson"
        exploreSpFromRm configValues [rm] []

let testExpandSpFromSp =
    fun _ ->
        let allSps = 
            scanStoredProcedureFiles configValues (
                fun spFile -> StoredProcedure ( spFile.getShortName(), spFile.Path) |> Some )                
        exploreSpFromSp configValues allSps []

let testClosureOfSps =
    fun _ ->
        let rm = ReadModel "FindPerson"
        closureOfSps configValues [rm] []

let testExpandEhcFromRm =
    fun _ ->
        let rm = ReadModel "FindPerson"
        exploreEhcFromRm configValues [rm] []

let testClosureOfIc =
    fun _ ->
        let rm = ReadModel "FindPerson"
        closureOfIcs configValues [rm] []

let testExpandIcFromSp =
    fun _ ->
        let sp = StoredProcedure ("ReadModel.PersonFinderWithCriteria", @"C:\Teleopti\Database\TeleoptiCCC7\Programmability\03StoredProcedures\ReadModel.PersonFinderWithCriteria.sql")
        exploreIcFromSp configValues [sp] []

let testClosureOfIc2 = 
    fun _ ->
        let rm = ReadModel "ScheduleProjectionReadOnly"
        closureOfIcs configValues [rm] []

let testDiscoverSpUsedInEh =
    fun _ ->
        let sps = getAllStoredProcedures configValues
        let ehcs = getAllEventHandlerClasses configValues
        [
            for sp in sps do
                for ehc in ehcs do
                    yield (sp, ehc)
        ]
        |> List.choose (fun (sp, ehc) -> discoverSpUsedInEh sp ehc)

let testDiscoverIcUsedInEh =
    fun _ ->
        let ic = InfraClass ("PersonScheduleDayReadModelPersister", @"C:\Teleopti\Infrastructure\Repositories\PersonScheduleDayReadModelPersister.cs")
        getAllEventHandlerClasses configValues
        |> List.choose (fun ehc -> discoverIcUsedInEh ic ehc)

let testDiscoverDcUsedInEh =
    fun _ ->
        let dc = DomainClass ("PersonScheduleDayReadModelsCreator", @"C:\Teleopti\Domain\ApplicationLayer\ScheduleChangedEventHandlers\PersonScheduleDayReadModel\PersonScheduleDayReadModelsCreator.cs")
        getAllEventHandlerClasses configValues
        |> List.choose (fun ehc -> discoverDcUsedInEh dc ehc)

