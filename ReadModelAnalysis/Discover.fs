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

let discoverWebClass (configValues : ConfigValues) (present : DiscoveryPresentation<'T>) : 'T list =
    let isClassFile (file : File') = 
        file.Path.EndsWith(".cs", System.StringComparison.OrdinalIgnoreCase)
    Dir'.toDir'(configValues.pathToWebClasses).getFiles(isClassFile)
    |> List.choose present

// open AssemblyDigest
// let discoverEventHandlingClass  (configValues : ConfigValues) (present : DiscoveryPresentation<'T>) : 'T list =

open CodeDigest

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

let discoverClassInterfacesForClassFile (className: string) (file : File') : string list =
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
   
let discoverIcUsedInIc (icTarget : InfraClass) (icHost: InfraClass) =
    if (icTarget = icHost)
    then None
    else 
        let (InfraClass (icTargetName, icTargetPath)) = icTarget
        let (InfraClass (icHostName, icHostPath)) = icHost
        let iocTypes = icTargetName :: (discoverClassInterfacesForClassFile icTargetName (File'.toFile' icTargetPath)) 
        let lines = File'.toFile'(icHostPath : string).getLines() 
        let mutable locs' : List<string * string> = []
        let mutable inClassBody = false
        let mutable instanceName = "DummyNonExistentInstance"
        let mutable currentMethod = ""
        lines |> List.iter ( 
            function    
            | ClassDefinitionPattern className -> inClassBody <- className = icHostName       
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
        | _ -> IcUsedInIc { target = icTarget; host = icHost; locs = _transformLocs locs'} |> Some

let discoverIcUsedInDc (ic : InfraClass) (dc: DomainClass) =   
    let (InfraClass (icName, icPath)) = ic
    let (DomainClass (dcName, dcPath)) = dc
    let iocTypes = icName :: (discoverClassInterfacesForClassFile icName (File'.toFile' icPath))    
    let lines = File'.toFile'(dcPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    let mutable inClassBody = false
    let mutable instanceName = "DummyNonExistentInstance"
    let mutable currentMethod = ""
    lines |> List.iter ( 
        function    
        | ClassDefinitionPattern className -> inClassBody <- className = dcName       
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
    | _ -> IcUsedInDc { target = ic; host = dc; locs = _transformLocs locs'} |> Some

let discoverDcUsedInDc (dcTarget : DomainClass) (dcHost: DomainClass) =
    if (dcTarget = dcHost)
    then None
    else 
        let (DomainClass (dcTargetName, dcTargetPath)) = dcTarget
        let (DomainClass (dcHostName, dcHostPath)) = dcHost
        let iocTypes = dcTargetName :: (discoverClassInterfacesForClassFile dcTargetName (File'.toFile' dcTargetPath)) 
        let lines = File'.toFile'(dcHostPath : string).getLines() 
        let mutable locs' : List<string * string> = []
        let mutable inClassBody = false
        let mutable instanceName = "DummyNonExistentInstance"
        let mutable currentMethod = ""
        lines |> List.iter ( 
            function    
            | ClassDefinitionPattern className -> inClassBody <- className = dcHostName       
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
        | _ -> DcUsedInDc { target = dcTarget; host = dcHost; locs = _transformLocs locs'} |> Some

let discoverIcUsedInWc (ic : InfraClass) (wc: WebClass) =   
    let (InfraClass (icName, icPath)) = ic
    let (WebClass (wcName, wcPath)) = wc
    let iocTypes = icName :: (discoverClassInterfacesForClassFile icName (File'.toFile' icPath))    
    let lines = File'.toFile'(wcPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    let mutable inClassBody = false
    let mutable instanceName = "DummyNonExistentInstance"
    let mutable currentMethod = ""
    lines |> List.iter ( 
        function    
        | ClassDefinitionPattern className -> inClassBody <- className = wcName       
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
    | _ -> IcUsedInWc { target = ic; host = wc; locs = _transformLocs locs'} |> Some

let discoverDcUsedInWc (dc : DomainClass) (wc: WebClass) =   
    let (DomainClass (dcName, dcPath)) = dc
    let (WebClass (wcName, wcPath)) = wc
    let iocTypes = dcName :: (discoverClassInterfacesForClassFile dcName (File'.toFile' dcPath))    
    let lines = File'.toFile'(wcPath : string).getLines() 
    let mutable locs' : List<string * string> = []
    let mutable inClassBody = false
    let mutable instanceName = "DummyNonExistentInstance"
    let mutable currentMethod = ""
    lines |> List.iter ( 
        function    
        | ClassDefinitionPattern className -> inClassBody <- className = wcName       
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
    | _ -> DcUsedInWc { target = dc; host = wc; locs = _transformLocs locs'} |> Some


let discoverWcUsedInWc (wcTarget : WebClass) (wcHost: WebClass) =
    if (wcTarget = wcHost)
    then None
    else 
        let (WebClass (wcTargetName, wcTargetPath)) = wcTarget
        let (WebClass (wcHostName, wcHostPath)) = wcHost
        let iocTypes = wcTargetName :: (discoverClassInterfacesForClassFile wcTargetName (File'.toFile' wcTargetPath)) 
        let lines = File'.toFile'(wcHostPath : string).getLines() 
        let mutable locs' : List<string * string> = []
        let mutable inClassBody = false
        let mutable instanceName = "DummyNonExistentInstance"
        let mutable currentMethod = ""
        lines |> List.iter ( 
            function    
            | ClassDefinitionPattern className -> inClassBody <- className = wcHostName       
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
        | _ -> WcUsedInWc { target = wcTarget; host = wcHost; locs = _transformLocs locs'} |> Some
