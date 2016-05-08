module Discover

open Config
open IO
open Domain
open CodeDigest
open AssemblyDigest

let _transformLocs (locs: List<string*string>) : LocInfo list =
    locs
    |> List.groupBy fst
    |> List.map (fun (k, v) -> { hostLoc = k;  targetLocs = v |> List.map snd |> List.distinct })

let discoverSqlQueryInNhibMapping (file : File') =
    let mutable queries : string list = []
    let lines = file.getLines() 
    lines |> List.iter (
        function
        | SqlQueryTagStartPattern queryName -> queries <- queryName :: queries
        | _ -> ()
    )
    queries |> List.map (fun q -> NhibQuery (q, file.Path))

let discoverRmUsedInNq (rm : ReadModel) (nq : NhibQuery) =
    let (NhibQuery (nqName, nqPath)) = nq
    let (ReadModel rmName) = rm
    let lines = File'.toFile'(nqPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    let mutable inQueryBody = false 
    lines |> List.iter ( 
        function
        | SqlQueryTagStartPattern queryName -> inQueryBody <- queryName = nqName              
        | ReadModelPattern rmName _ -> 
            if (inQueryBody) then locs' <- (nqName, rmName) :: locs'
        | _ -> ())   
    match locs' with
    | [] -> None
    | _ -> RmUsedInNq { target = rm; host = nq; locs = _transformLocs locs'} |> Some
    
let discoverRmUsedInSp (rm : ReadModel) (sp : StoredProcedure) =
    let (StoredProcedure (spName, spPath)) = sp
    let (ReadModel rmName) = rm
    let lines = File'.toFile'(spPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    lines |> List.iter ( 
        function     
        | ReadModelPattern rmName _ -> locs' <- (spName, rmName) :: locs'
        | _ -> ())   
    match locs' with
    | [] -> None
    | _ -> RmUsedInSp { target = rm; host = sp; locs = _transformLocs locs'} |> Some

let discoverRmUsedInIc (rm : ReadModel) (ic : InfraClass) =
    let (InfraClass (icName, icPath)) = ic
    let (ReadModel rmName) = rm
    let lines = File'.toFile'(icPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    let mutable inClassBody = false
    let mutable currentMethod = ""
    lines |> List.iter ( 
        function    
        | ClassDefinitionPattern className -> inClassBody <- className = icName       
        | MethodDefinitionPattern methodName ->
            if (inClassBody)
            then currentMethod <- methodName
        | ReadModelPattern rmName _ -> 
            if (inClassBody)
            then  locs' <- (currentMethod, rmName) :: locs'           
        | _ -> ())   
    match locs' with
    | [] -> None
    | _ -> RmUsedInIc { target = rm; host = ic; locs = _transformLocs locs'} |> Some

let _discoverSpUsedInClass (target: StoredProcedure ) (host, hostName, hostPath) (buildUsage: UseInfo<StoredProcedure, 'U> -> Usage) = 
    let (StoredProcedure (spName, spPath)) = target
    let lines = File'.toFile'(hostPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    let mutable inClassBody = false
    let mutable currentMethod = ""
    lines |> List.iter ( 
        function    
        | ClassDefinitionPattern className -> inClassBody <- className = hostName       
        | MethodDefinitionPattern methodName ->
            if (inClassBody)
            then currentMethod <- methodName
        | StoredProcedurePattern spName _ -> 
            if (inClassBody)
            then  locs' <- (currentMethod, spName) :: locs'           
        | _ -> ())   
    match locs' with
    | [] -> None
    | _ -> buildUsage { target = target; host = host; locs = _transformLocs locs'} |> Some

let discoverSpUsedInDc (target : StoredProcedure) (host : DomainClass) =
    let hostTriple = host, Q.name host, Q.path host
    _discoverSpUsedInClass target hostTriple SpUsedInDc

let discoverSpUsedInIc (target : StoredProcedure) (host : InfraClass) =
    let hostTriple = host, Q.name host, Q.path host
    _discoverSpUsedInClass target hostTriple SpUsedInIc

let discoverSpUsedInSp (target : StoredProcedure) (host : StoredProcedure) =
    if (target = host)
    then None
    else 
        let (StoredProcedure (targetName, targetPath)) = target
        let (StoredProcedure (hostName, hostPath)) = host
        let lines = File'.toFile'(hostPath : string).getLines() 
        let mutable locs' : List<string * string> = []
        lines |> List.iter ( 
            function     
            | StoredProcedurePattern targetName _ -> locs' <- (hostName, targetName) :: locs'
            | _ -> ())   
        match locs' with
        | [] -> None
        | _ -> SpUsedInSp { target = target; host = host; locs = _transformLocs locs'} |> Some

let _discoverNqUsedInClass (target : NhibQuery) (host, hostName, hostPath) (buildUsage : UseInfo<NhibQuery, 'T> -> Usage) =  
    let (NhibQuery (targetName, targetPath)) = target
    let lines = File'.toFile'(hostPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    let mutable inClassBody = false
    let mutable currentMethod = ""
    lines |> List.iter ( 
        function    
        | ClassDefinitionPattern className -> inClassBody <- className = hostName       
        | MethodDefinitionPattern methodName ->
            if (inClassBody)
            then currentMethod <- methodName
        | NhibQueryPattern targetName _ -> 
            if (inClassBody)
            then  locs' <- (currentMethod, targetName) :: locs'           
        | _ -> ())   
    match locs' with
    | [] -> None
    | _ -> buildUsage { target = target; host = host; locs = _transformLocs locs'} |> Some

let discoverNqUsedInDc (target : NhibQuery) (host: DomainClass) =
    let hostTriple = host, Q.name host, Q.path host
    _discoverNqUsedInClass target hostTriple NqUsedInDc

let discoverNqUsedInIc (target : NhibQuery) (host: InfraClass) =
    let hostTriple = host, Q.name host, Q.path host
    _discoverNqUsedInClass target hostTriple NqUsedInIc

let _discoverClassInterfacesForClassFile (className: string) (file : File') : string list =
    let lines = file.getLines()
    let classDefinitionIndex = lines |> List.findIndex ( function | ClassDefinitionPattern className' -> className = className' | _ -> false )
    let (_ , classDefinitionLines) = lines |> List.splitAt classDefinitionIndex              
    let rec extractClassDefinitionStatement lines classDefinitionStatement = 
        match lines with
        | [] -> ""
        | line :: lines' ->
            match line with          
            | BlockStartPattern _ -> classDefinitionStatement + line               
            | _ ->  extractClassDefinitionStatement lines' (classDefinitionStatement + line)                      
    let classDefinitionStatement = extractClassDefinitionStatement classDefinitionLines ""
    extractInterfacesFromString classDefinitionStatement
   
let _discoverClassUsedInClass (target, targetName, targetPath) (host, hostName, hostPath) (buildUsage: UseInfo<'T, 'U> -> Usage) =
    let iocTypes = targetName :: (_discoverClassInterfacesForClassFile targetName <| File'.toFile' (targetPath : string))
    let lines = File'.toFile'(hostPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    let mutable inClassBody = false
    let mutable instanceName = "DummyNonExistentInstance"
    let mutable currentMethod = "" 
    lines |> List.iter ( 
        function    
        | ClassDefinitionPattern className -> inClassBody <- className = hostName       
        | MethodDefinitionPattern methodName ->
            if (inClassBody)
            then currentMethod <- methodName
        | InterfaceOrClassInstancePattern iocTypes instanceName' -> 
            if (inClassBody)
            then instanceName <- instanceName' 
        | InstanceMethodInvocation instanceName invocationName ->
            if (inClassBody)
            then locs' <- (currentMethod, invocationName) :: locs'           
        | _ -> ())   
    match locs' with
    | [] -> None
    | _ -> buildUsage {target = target; host = host; locs = _transformLocs locs'} |> Some

let discoverDcUsedInDc (target : DomainClass) (host: DomainClass) =
    if (target = host)
    then None
    else
        let targetTriple = target, Q.name target, Q.path target
        let hostTriple = host, Q.name host, Q.path host
        _discoverClassUsedInClass targetTriple hostTriple DcUsedInDc

let discoverIcUsedInIc (target : InfraClass) (host: InfraClass) =
    if (target = host)
    then None
    else
        let targetTriple = target, Q.name target, Q.path target
        let hostTriple = host, Q.name host, Q.path host
        _discoverClassUsedInClass targetTriple hostTriple IcUsedInIc

let discoverWcUsedInWc (target : WebClass) (host: WebClass) =
    if (target = host)
    then None
    else 
        let targetTriple = target, Q.name target, Q.path target
        let hostTriple = host, Q.name host, Q.path host
        _discoverClassUsedInClass targetTriple hostTriple WcUsedInWc

let discoverIcUsedInDc (target : InfraClass) (host: DomainClass) =   
    let targetTriple = target, Q.name target, Q.path target
    let hostTriple = host, Q.name host, Q.path host
    _discoverClassUsedInClass targetTriple hostTriple IcUsedInDc

let discoverDcUsedInWc (target : DomainClass) (host: WebClass) =  
    let targetTriple = target, Q.name target, Q.path target
    let hostTriple = host, Q.name host, Q.path host
    _discoverClassUsedInClass targetTriple hostTriple DcUsedInWc 

let discoverIcUsedInWc (target : InfraClass) (host: WebClass) =   
    let targetTriple = target, Q.name target, Q.path target
    let hostTriple = host, Q.name host, Q.path host
    _discoverClassUsedInClass targetTriple hostTriple IcUsedInWc

let _discoverEventsPublishInClass (es : EventClass list) (host, hostName, hostPath) =
    let enames = es |> List.map (function EventClass ename -> ename)
    let lines = File'.toFile'(hostPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    let mutable inClassBody = false
    let mutable currentMethod = "" 
    lines |> List.iter ( 
        function    
        | ClassDefinitionPattern className -> inClassBody <- className = hostName       
        | MethodDefinitionPattern methodName ->
            if (inClassBody)
            then currentMethod <- methodName
        | PublishEventPattern enames eventName ->
            if (inClassBody)
            then locs' <- (currentMethod, eventName) :: locs'                 
        | _ -> ())   
    match locs' with
    | [] -> None
    | _ -> locs' |> Some

let _discoverEhcHandlesClass (target: EventHandlerClass) (host, hostName, hostPath) (buildUsage: EhandleInfo<'T> -> Usage) =
    let publishLocs' = _discoverEventsPublishInClass (Q.events target) (host, hostName, hostPath)               
    publishLocs' 
    |> Option.map (fun locs' ->
        buildUsage {handler = target; publisher = host; locs = _transformLocs locs'})
   
let discoverEhcHandlesIc (target: EventHandlerClass) (host: InfraClass) =
     let hostTriple = host, Q.name host, Q.path host
     _discoverEhcHandlesClass target hostTriple EhcHandlesIc

let discoverEhcHandlesDc (target: EventHandlerClass) (host: DomainClass) =
    let hostTriple = host, Q.name host, Q.path host
    _discoverEhcHandlesClass target hostTriple EhcHandlesDc

let discoverEhcHandlesWc (target: EventHandlerClass) (host: WebClass) =
    let hostTriple = host, Q.name host, Q.path host
    _discoverEhcHandlesClass target hostTriple EhcHandlesWc

let discoverEhcHandlesEhc (target: EventHandlerClass) (host: EventHandlerClass) =
    if (target = host)
    then None
    else 
        let targetName, targetPath, targetEvents = Q.name target, Q.path target, Q.events target
        let targetEnames = targetEvents |> List.map (function EventClass ename -> ename)
        let hostName, hostPath = Q.name host, Q.path host
        let lines = File'.toFile'(hostPath : string).getLines() 
        let mutable locs' : List<string * string> = []
        let mutable inClassBody = false
        let mutable currentHandleEvent = "" 
        lines |> List.iter ( 
            function    
            | ClassDefinitionPattern className -> inClassBody <- className = hostName       
            | HandleMethodDefinitionPattern eventName ->
                if (inClassBody)
                then currentHandleEvent <- eventName
            | PublishEventPattern targetEnames eventName ->
                if (inClassBody)
                then locs' <- (currentHandleEvent, eventName) :: locs'                 
            | _ -> ())   
        match locs' with
        | [] -> None
        | _ -> EhcHandlesEhc { handler = target; publisher = host; locs = _transformLocs locs' } |> Some