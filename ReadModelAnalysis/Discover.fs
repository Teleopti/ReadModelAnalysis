module Discover

open Config
open IO
open Domain

type DiscoveryPresentation<'T> = File' -> 'T option

let discoverNhibMapping (configValues : ConfigValues) (present : DiscoveryPresentation<'T>) : 'T list =
    let isNhibMappingFile (file : File') = 
        file.Path.EndsWith(".hbm.xml", System.StringComparison.OrdinalIgnoreCase)
    Dir'.toDir'(configValues.pathToNhibMappings).getFiles(isNhibMappingFile)   
    |> List.choose present

let discoverStoredProcedure (configValues : ConfigValues) (present : DiscoveryPresentation<'T>) : 'T list =
    let isStoredProcedureFile (file : File') = 
        file.Path.EndsWith(".sql", System.StringComparison.OrdinalIgnoreCase)
    Dir'.toDir'(configValues.pathToStoredProcedures).getFiles(isStoredProcedureFile)
    |> List.choose present

let discoverInfraClass (configValues : ConfigValues) (present : DiscoveryPresentation<'T>) : 'T list =
    let isClassFile (file : File') = 
        file.Path.EndsWith(".cs", System.StringComparison.OrdinalIgnoreCase)
    Dir'.toDir'(configValues.pathToInfraClasses).getFiles(isClassFile)
    |> List.choose present

let discoverDomainClass (configValues : ConfigValues) (present : DiscoveryPresentation<'T>) : 'T list =
    let isClassFile (file : File') = 
        file.Path.EndsWith(".cs", System.StringComparison.OrdinalIgnoreCase)
    Dir'.toDir'(configValues.pathToDomainClasses).getFiles(isClassFile)
    |> List.choose present

open ContentDigest

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

let discoverClassInFile (builder: string * string -> 'T) (file : File') =
    let mutable classes : string list = []
    let lines = file.getLines() 
    lines |> List.iter (
        function
        | ClassDefinitionPattern className -> classes <- className :: classes
        | _ -> ()
    )
    classes |> List.map (fun c -> builder (c, file.Path))


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

let discoverSpUsedInSp (spTarget : StoredProcedure) (spHost : StoredProcedure) =
    if (spTarget = spHost)
    then None
    else 
        let (StoredProcedure (spNameTarget, spPathTarget)) = spTarget
        let (StoredProcedure (spNameHost, spPathHost)) = spHost
        let lines = File'.toFile'(spPathHost : string).getLines() 
        let mutable locs' : List<string * string> = []
        lines |> List.iter ( 
            function     
            | StoredProcedurePattern spNameTarget _ -> locs' <- (spNameHost, spNameTarget) :: locs'
            | _ -> ())   
        match locs' with
        | [] -> None
        | _ -> SpUsedInSp { target = spTarget; host = spHost; locs = _transformLocs locs'} |> Some

let discoverSpUsedInIc (sp : StoredProcedure) (ic : InfraClass) =
    let (InfraClass (icName, icPath)) = ic
    let (StoredProcedure (spName, spPath)) = sp
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
        | StoredProcedurePattern spName _ -> 
            if (inClassBody)
            then  locs' <- (currentMethod, spName) :: locs'           
        | _ -> ())   
    match locs' with
    | [] -> None
    | _ -> SpUsedInIc { target = sp; host = ic; locs = _transformLocs locs'} |> Some

let discoverSpUsedInDc (sp : StoredProcedure) (dc : DomainClass) =
    let (DomainClass (dcName, dcPath)) = dc
    let (StoredProcedure (spName, spPath)) = sp
    let lines = File'.toFile'(dcPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    let mutable inClassBody = false
    let mutable currentMethod = ""
    lines |> List.iter ( 
        function    
        | ClassDefinitionPattern className -> inClassBody <- className = dcName       
        | MethodDefinitionPattern methodName ->
            if (inClassBody)
            then currentMethod <- methodName
        | StoredProcedurePattern spName _ -> 
            if (inClassBody)
            then  locs' <- (currentMethod, spName) :: locs'           
        | _ -> ())   
    match locs' with
    | [] -> None
    | _ -> SpUsedInDc { target = sp; host = dc; locs = _transformLocs locs'} |> Some

let discoverNqUsedInDc (nq : NhibQuery) (dc: DomainClass) =
    let (DomainClass (dcName, dcPath)) = dc
    let (NhibQuery (nqName, nqPath)) = nq
    let lines = File'.toFile'(dcPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    let mutable inClassBody = false
    let mutable currentMethod = ""
    lines |> List.iter ( 
        function    
        | ClassDefinitionPattern className -> inClassBody <- className = dcName       
        | MethodDefinitionPattern methodName ->
            if (inClassBody)
            then currentMethod <- methodName
        | NhibQueryPattern nqName _ -> 
            if (inClassBody)
            then  locs' <- (currentMethod, nqName) :: locs'           
        | _ -> ())   
    match locs' with
    | [] -> None
    | _ -> NqUsedInDc { target = nq; host = dc; locs = _transformLocs locs'} |> Some

let discoverNqUsedInIc (nq : NhibQuery) (ic: InfraClass) =
    let (InfraClass (icName, icPath)) = ic
    let (NhibQuery (nqName, nqPath)) = nq
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
        | NhibQueryPattern nqName _ -> 
            if (inClassBody)
            then  locs' <- (currentMethod, nqName) :: locs'           
        | _ -> ())   
    match locs' with
    | [] -> None
    | _ -> NqUsedInIc { target = nq; host = ic; locs = _transformLocs locs'} |> Some