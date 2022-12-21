#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus
open System.Text.RegularExpressions

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function 
    | _::s::_ -> Some s 
    | _ -> None

let readFile fileName =
    try
        File.ReadLines fileName
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

type Shout =
    | Value of int
    | Operation of {|Var1:string; Var2:string; Operator: char|}

let parseInput (strings: string seq) = 
    strings
    |> Seq.map (fun str ->
        let valueMatch = Regex.Match(str, "([a-z]+): ([0-9]+)")
        let opMatch = Regex.Match(str, "([a-z]+): ([a-z]+) (.) ([a-z]+)")
        if valueMatch.Success then 
            (valueMatch.Groups[1].Value, 
            (int valueMatch.Groups[2].Value) |> Value )
        elif opMatch.Success then 
            (opMatch.Groups[1].Value,  
            {| Var1 = opMatch.Groups[2].Value; Operator = opMatch.Groups[3].Value[0] ; Var2 = opMatch.Groups[4].Value |} |> Operation )
        else failwith "Failed parse"
    )
    |> Map.ofSeq

let toCode (apor: Map<string, Shout>) =
    let rec code apa =
        match apor[apa] with
        | Value i -> [$"let {apa} = {i}L"]
        | Operation op ->
            code op.Var1 @ code op.Var2 @ [$"let {apa} = {op.Var1} {op.Operator} {op.Var2}"]
    "open System"::code "root" @ ["Console.WriteLine $\"Answer {root}\""]

let humnDependencies (apor: Map<string, Shout>) =
    let rec go acc apa =
        match apor[apa] with
        | Value _i -> Set.empty
        | Operation op when op.Var1 = "humn" || op.Var2 = "humn" ->
            Set.add apa acc
        | Operation op ->
            acc |> Set.union (go acc op.Var1) |> Set.union (go acc op.Var2)

    go Set.empty "root"

let part1 input =
    parseInput input
    |> toCode
    |> fun  code -> File.WriteAllLines("part1.fsx", code)
    "Written"

let part2 input =
    let apor = parseInput input
    let humnDependencies = humnDependencies apor
    let root = 
        apor["root"]
        |> function
            | Operation op when Set.contains op.Var2 humnDependencies ->
                {| Var1 = op.Var2; Var2 = op.Var1; Operator= op.Operator |} |> Operation
            | Operation op -> Operation op
            | Value _ -> failwith "Root was value"
    "Written"

module Tests =
    let private tests = 
        [
            //fun () -> Error "todo"
        ]
    let run () =
        tests 
        |> List.fold (fun state test -> state |> Result.bind test ) (Ok ())
        |>> fun () -> "All tests Ok"

let input = fileName |>> readFile
match input with
| Some input ->
    Result.map3
        (sprintf "Successful run!\r\nTests: %s\r\nPart1: %s\r\nPart2: %s")
        (Tests.run ())
        (input |>> part1)
        (input |>> part2)
| None ->
    Tests.run () |>> sprintf "Tests only:\r\n %s"
|> function
| Ok s -> s
| Error s -> sprintf "Error: %s" s 
|> Console.WriteLine 
