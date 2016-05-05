﻿module ContentDigest

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
    let pattern = String.Join(@"\.", pieces) 
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

let (|MethodDefinitionPattern|_|) line = 
    let pattern = @"(?:(public|protected|private|static|virtual|override)\s+)+(?<returnType>\w+(?:<\w+>)?)\s+(?<methodName>\w+)\s*\("
    let matched = Regex.Match(line, pattern, RegexOptions.IgnoreCase)
    if matched.Success then Some(matched.Groups.["methodName"].Value) else None

    