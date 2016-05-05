module AssemblyDigest

open System
open System.Reflection
open Config

let loadDomain (config : ConfigValues) =    
    Assembly.LoadFrom(config.pathToDomainAssembly)

let checkEventHandlerType (t : Type) =
    let signature = @"IHandleEvent`1"    
    let events =
        t.GetInterfaces()
        |> Seq.choose (fun i ->
            if (i.Name = signature)
            then i.GetGenericArguments().[0].Name |> Some
            else None)
        |> Seq.toList
    if (List.length events = 0)
    then None
    else Some events
    
let domainClassTypeToPath (config : ConfigValues) (t : Type) =
    let domainNsPrefix = config.domainNsPrefix
    [ 
        yield config.pathToDomainClasses
        yield! Array.toList <|  t.FullName.Replace(domainNsPrefix, "").Split('.')           
    ]
    |> fun pieces -> String.Join("\\", pieces) + ".cs"
      