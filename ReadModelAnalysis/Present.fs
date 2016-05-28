module Present

open Domain
open Microsoft.FSharp.Reflection

type EntityInfo = { typeName: string; name: string; detail: string }
type UsageInfo = { target: EntityInfo; host: EntityInfo; detail: string}

let getEntityInfo e =
    let info' = FSharpValue.GetUnionFields (e, null)
    let typeName = info' |> fst |> fun t -> t.Name
    let name = info' |> snd |> (fun os -> os.[0] :?> string )
    let detail = sprintf "%80A" e
    { typeName = typeName; name = name; detail = detail}

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
    let detail = sprintf "%100A" usage
    { target = target; host = host; detail = detail}

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
        |> List.mapi ( fun i piece -> if i = 0 then sprintf "%s [%s]" piece locInfo else piece)
        |> Some

let formatUsageChainForWebClass usageChain =
    _formatUsageChainForWebClassInternal usageChain 
    |> Option.map (fun x -> x |> String.concat " -> ")