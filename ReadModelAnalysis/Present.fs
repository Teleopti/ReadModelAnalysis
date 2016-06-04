module Present

open Domain
open Microsoft.FSharp.Reflection

type EntityInfo = { typeName: string; name: string }
type UsageInfo = { target: EntityInfo; host: EntityInfo}

let getEntityInfo e =
    let info' = FSharpValue.GetUnionFields (e, null)
    let typeName = info' |> fst |> fun t -> t.Name
    let name = info' |> snd |> (fun os -> os.[0] :?> string )
    { typeName = typeName; name = name}

let getWebLocInfo usage = 
    let formatLocs locs = 
        locs |> List.map (fun x -> x.hostLoc) |> String.concat ", "
    match usage with
    | DcUsedInWc { target = target'; host = host'; locs = locs' } -> formatLocs locs' |> Some
    | IcUsedInWc { target = target'; host = host'; locs = locs' } -> formatLocs locs' |> Some
    | WcUsedInWc { target = target'; host = host'; locs = locs' } -> formatLocs locs' |> Some
    | EhcHandlesWc { handler = handler'; publisher = publisher'; locs = locs' } -> formatLocs locs' |> Some
    | _ -> None

let getUsageInfo usage = 
    let info' = 
        FSharpValue.GetUnionFields (usage, null) |> snd 
        |> (fun i -> FSharpValue.GetRecordFields(i.[0])) 
    let target = info'.[0] |> getEntityInfo
    let host = info'.[1] |> getEntityInfo
    { target = target; host = host}

let _formatUsageChainInternal  (usageChain : UsageChain) =    
    let stringify o = sprintf "%s (%s)" o.name o.typeName         
    usageChain 
    |> List.map getUsageInfo 
    |> List.collect (fun i -> [ stringify i.target; stringify i.host ]) 
    |> List.distinct |> List.rev 

let _formatUsageChainForWebClassInternal usageChain =
    match List.last usageChain |> getWebLocInfo with
    | None -> None
    | Some locInfo ->
        _formatUsageChainInternal usageChain
        |> List.mapi ( fun i piece -> if i = 0 then sprintf "%s [%s]" piece locInfo else "\t <- " + piece)
        |> Some

let formatUsageChainForWebClass usageChain =
    _formatUsageChainForWebClassInternal usageChain 
    |> Option.map (fun x -> x |> String.concat "\r\n")

let presentInFile filePath usageChains usages =
    use writer = new System.IO.StreamWriter(filePath, append = false)            
    usageChains
    |> List.choose  formatUsageChainForWebClass  
    |> List.iter (fprintfn writer "%s")
    fprintfn writer "\r\n\r\n---------------------------------\r\n\r\n"
    usages
    |> List.iter (fprintfn writer "%90A")    
    
     