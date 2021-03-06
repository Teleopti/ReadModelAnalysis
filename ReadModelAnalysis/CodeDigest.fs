﻿module CodeDigest

open System
open System.Text.RegularExpressions   

let (|ReadModelPattern|_|) readModel line = 
    let pattern = @"(?:\[?\bReadModel\]?\.)" + @"\[?\b" + Regex.Escape(readModel) + @"\b\]?"
    let matched = Regex.Match(line, pattern, RegexOptions.IgnoreCase)
    if matched.Success then Some(readModel) else None

let (|SqlQueryTagStartPattern|_|) line =
    let pattern = """^\s*<sql-query.*name="(?<name>\w+)".*>\s*$"""
    let matched = Regex.Match(line, pattern, RegexOptions.IgnoreCase)
    if matched.Success then Some(matched.Groups.["name"].Value) else None

let (|StoredProcedurePattern|_|) storedProcedureName line = 
    let pieces = Regex.Split(storedProcedureName, @"\.") |> Seq.map (fun s -> @"\[?" + s + @"\]?")
    let pattern = @"\b" + String.Join(@"\.", pieces) + @"\b"
    let matched = Regex.Match(line, pattern, RegexOptions.IgnoreCase)
    if matched.Success then Some(storedProcedureName) else None

let (|NhibQueryPattern|_|) nhibMappingName line = 
    let pattern = """GetNamedQuery\(\s*"\s*""" + Regex.Escape(nhibMappingName) + """\s*"\)"""
    let matched = Regex.Match(line, pattern)
    if matched.Success then Some(nhibMappingName) else None

let (|ClassDefinitionPattern|_|) line = 
    let pattern = @"^\s*(?:(public|protected|private|static)\s+)+class\s+(?<className>\w+)"
    let matched = Regex.Match(line, pattern, RegexOptions.IgnoreCase)
    if matched.Success then Some(matched.Groups.["className"].Value) else None

let (|InterfaceDefinitionPattern|_|) line = 
    let pattern = @"^\s*(?:(public|protected|private|static)\s+)+interface\s+(?<interfaceName>\w+)"
    let matched = Regex.Match(line, pattern, RegexOptions.IgnoreCase)
    if matched.Success then Some(matched.Groups.["interfaceName"].Value) else None

let (|BlockStartPattern|_|) line = 
    let pattern = @"{\s*$"
    let matched = Regex.Match(line, pattern, RegexOptions.IgnoreCase)
    if matched.Success then Some(line) else None

let (|MethodDefinitionPattern|_|) line = 
    let pattern = @"(?:(public|protected|private|static|virtual|override)\s+)+(?<returnType>\w+(?:<.+>)?)\s+(?<methodName>\w+)\s*\("
    let matched = Regex.Match(line, pattern, RegexOptions.IgnoreCase)
    if matched.Success then Some(matched.Groups.["methodName"].Value) else None

let (|InterfaceOrClassInstancePattern|_|) inames line = 
    match inames with 
    | [] -> None
    | _ -> 
        let wrapped = "(" + String.Join("|", inames |> List.map (fun x -> Regex.Escape(x))) + ")"
        let pattern = @"^\s*(?:(public|protected|private|readonly|static)\s+)+" + wrapped + @"\s+(?<instanceName>\w+)\b\s*;"
        let matched = Regex.Match(line, pattern, RegexOptions.IgnoreCase)
        if matched.Success then Some(matched.Groups.["instanceName"].Value) else None

let (|InstanceMethodInvocation|_|) instanceName line = 
    let pattern = @"\b" + Regex.Escape(instanceName ) + @"\.(?<methodName>\w+)"  + @"\b\s*\("
    let matched = Regex.Match(line, pattern)
    if matched.Success then Some(matched.Groups.["methodName"].Value) else None

let (|NamespacePattern|_|) line = 
    let pattern = @"^namespace\s+(?<namespace>[\w.]+)\s*$"
    let matched = Regex.Match(line, pattern)
    if matched.Success then Some(matched.Groups.["namespace"].Value) else None

let (|PublishEventPattern|_|) enames line =
    match enames with
    | [] -> None
    | _ -> 
        let wrapped = "(?<eventName>" + String.Join("|", enames |> List.map (fun x -> Regex.Escape(x))) + ")"
        let pattern = @"\bnew\s+" + wrapped + @"\b"
        let matched = Regex.Match(line, pattern)
        if matched.Success then Some(matched.Groups.["eventName"].Value) else None

let (|HandleMethodDefinitionPattern|_|) line = 
    let pattern = @"(?:(public|protected|private|static|virtual|override)\s+)+void\s+Handle\s*\(\s*(?<eventType>\w+)\s+"
    let matched = Regex.Match(line, pattern, RegexOptions.IgnoreCase)
    if matched.Success then Some(matched.Groups.["eventType"].Value) else None

let extractInterfacesFromString input =
    let pattern = @"(?<=\s)I[\w]+(<.+>)?";
    let matches = Regex.Matches(input, pattern);
    matches |> Seq.cast<Match> |> Seq.toList |> List.map (fun m -> m.Value)

let (|MethodInvocationPattern|_|) (methodNames : string list) line = 
    match methodNames with
    | [] -> None
    | _ ->
        let pattern = @"(?<=\s)(?<methodName>" + String.Join("|", methodNames) + @")\b\s*[(]"
        let matched = Regex.Match(line, pattern)
        if matched.Success then Some(matched.Groups.["methodName"].Value) else None


open IO
open Config
    
type FilePresentation<'T> = File' -> 'T option

let scanNhibMappingFiles (configValues : ConfigValues) (present : FilePresentation<'T>) : 'T list =
    let isNhibMappingFile (file : File') = 
        file.Path.EndsWith(".hbm.xml", System.StringComparison.OrdinalIgnoreCase)
    Dir'.toDir'(configValues.pathToNhibMappings).getFiles(isNhibMappingFile)   
    |> List.choose present

let scanStoredProcedureFiles (configValues : ConfigValues) (present : FilePresentation<'T>) : 'T list =
    let isStoredProcedureFile (file : File') = 
        file.Path.EndsWith(".sql", System.StringComparison.OrdinalIgnoreCase)
    Dir'.toDir'(configValues.pathToStoredProcedures).getFiles(isStoredProcedureFile)
    |> List.choose present

let scanInfraClassFiles (configValues : ConfigValues) (present : FilePresentation<'T>) : 'T list =
    let isClassFile (file : File') = 
        file.Path.EndsWith(".cs", System.StringComparison.OrdinalIgnoreCase)
    Dir'.toDir'(configValues.pathToInfraClasses).getFiles(isClassFile)
    |> List.choose present

let scanDomainClassFiles (configValues : ConfigValues) (present : FilePresentation<'T>) : 'T list =
    let isClassFile (file : File') = 
        file.Path.EndsWith(".cs", System.StringComparison.OrdinalIgnoreCase)
    Dir'.toDir'(configValues.pathToDomainClasses).getFiles(isClassFile)
    |> List.choose present

let scanWebClassFiles (configValues : ConfigValues) (present : FilePresentation<'T>) : 'T list =
    let isClassFile (file : File') = 
        file.Path.EndsWith(".cs", System.StringComparison.OrdinalIgnoreCase)
    Dir'.toDir'(configValues.pathToWebClasses).getFiles(isClassFile)
    |> List.choose present

let scanClassFiles (dir: Dir') (present : FilePresentation<'T>) : 'T list =
    let isClassFile (file : File') = 
        file.Path.EndsWith(".cs", System.StringComparison.OrdinalIgnoreCase)
    dir.getFiles(isClassFile)
    |> List.choose present

let discoverSqlQueryInNhibMapping (file : File') =
    let mutable queries : string list = []
    let lines = file.getLines() 
    lines |> List.iter (
        function
        | SqlQueryTagStartPattern queryName -> queries <- queryName :: queries
        | _ -> ()
    )
    queries |> List.map (fun q -> Domain.NhibQuery (q, file.Path))

let discoverClassInFile (builder: string * string -> 'T) (file : File') =
    let mutable classes : string list = []
    let lines = file.getLines() 
    lines |> List.iter (
        function
        | ClassDefinitionPattern className -> classes <- className :: classes
        | _ -> ()
    )
    classes |> List.map (fun c -> builder (c, file.Path))

let getAllStoredProcedures configValues =
    scanStoredProcedureFiles configValues (
        fun afile -> Domain.StoredProcedure ( afile.getShortName(), afile.Path) |> Some)

let getAllNhibQueries configValues =
    scanNhibMappingFiles configValues (discoverSqlQueryInNhibMapping >> Some) |> List.concat

let getAllInfraClasses configValues = 
    scanInfraClassFiles configValues (discoverClassInFile Domain.InfraClass >> Some) |> List.concat

let getAllWebClasses configValues =
    scanWebClassFiles configValues (discoverClassInFile Domain.WebClass >> Some) |> List.concat

let getAllReadModels =
    ["AdherenceDetails"; "AdherencePercentage"; "FindPerson"; "GroupingReadOnly"; "PersonScheduleDay";
        "PersonScheduleProjectionLoadTime"; "ScheduleDay"; "ScheduleProjectionReadOnly";
        "SiteOutOfAdherence"; "TeamOutOfAdherence"; ]
    |> List.map Domain.ReadModel