#load "IO.fs"
#load "Domain.fs"
#load "Config.fs"
#load "CodeDigest.fs"

open IO
open Domain
open Config
open CodeDigest

let testGetAllNhibQueries =
    fun () ->
        getAllNhibQueries configValues

let testMethodDefinitionPattern =
    fun () ->
        let s = @"  public IEnumerable<Tuple<string, IEnumerable<DateOnlyPeriod>>> ExistingForecastForAllSkills(DateOnlyPeriod range,"
        match s with 
        | MethodDefinitionPattern methodName -> Some methodName
        | _ -> None
        