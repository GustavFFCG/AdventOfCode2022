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
    | Equality of string

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
        | Equality _ -> failwith "Not handled in part 1"
    "open System"::code "root" @ ["Console.WriteLine $\"Answer {root}\""]

let toCode2 (apor: Map<string, Shout>) =

    let reverseOp x switch y op z  =
        //Console.WriteLine $"Reversing {x} = {y} {op} {z} on {switch}"
        if x = "root" then
            switch, Equality (if switch = y then z else y)
        else
            let var1, op , var2 =
                match op with 
                | '+' -> if switch = y then x, '-', z else x, '-', y
                | '-' -> if switch = y then z, '+', x else y, '-', x
                | '*' -> if switch = y then x, '/', z else x, '/', y
                | '/' -> if switch = y then z, '*', x else y, '/', x
                | c -> failwith $"unexpected operator {c}"
            switch, Operation {| Var1 = var1 ; Operator = op; Var2 = var2 |}

    let rec code reverse apa =
        let (var, shout), reversed =
            if reverse then
                apor 
                |> Map.findKey 
                    (fun _key (s: Shout) -> 
                        match s with | Operation op when op.Var1 = apa || op.Var2 = apa -> true | _ -> false
                    )
                |> (fun key ->
                    apor[key]
                    |> function 
                    | Operation op ->
                        if op.Var1 = apa then reverseOp key op.Var1 op.Var1 op.Operator op.Var2, Some key
                        else reverseOp key op.Var2 op.Var1 op.Operator op.Var2, Some key
                    | _ -> failwith "cannot reverse value or equality" 
                )
            else
                (apa, apor[apa]), None
        match shout with
        | Value i -> [$"let {var} = {i}L"]
        | Equality eq ->
            code false eq 
            @ [$"let {var} = {eq}"]
        | Operation op ->
            (if reverse then
                if reversed = Some op.Var1 then
                    code true op.Var1
                    @ code false op.Var2 
                else
                    code true op.Var2
                    @ code false op.Var1
            else
                code false op.Var1
                @ code false op.Var2 
            )
            //code false op.Var1
            //@ code reverse op.Var2 
            @ [$"let {var} = {op.Var1} {op.Operator} {op.Var2}"]
    
    "open System"::(code true "humn") @ ["Console.WriteLine $\"Answer {humn}\""]

let part1 input =
    parseInput input
    |> toCode
    |> fun  code -> File.WriteAllLines("part1.fsx", code)
    "Written"

let part2 input =
    parseInput input
    |> toCode2
    |> fun  code -> File.WriteAllLines("part2.fsx", code)
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
