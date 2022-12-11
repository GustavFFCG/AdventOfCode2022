#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus
open System.Text.RegularExpressions

let primeproduct =
    [2L;3L;5L;7L;11L;13L;17L;19L;23L]
    |> List.fold (fun a b -> a * b) 1L
Console.WriteLine $"prod {primeproduct}"

type Monkey = {
    Items: int64 list
    Operation: int64 -> int64
    Test: int64 -> int
    Inspections: int64
}

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function 
    | _::s::_ -> Ok s 
    | _ -> Error "Please provide a filename for input"

let readFile fileName =
    try
        File.ReadLines fileName
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let startItems str : int64 seq =
    let m = Regex.Match(str, "Starting items: ([0-9, ]*)")
    if m.Success then m.Groups[1].Value |> String.split [", "] |>> int64
    else failwith "not starting items"

let operation (reducer: (int64->int64)) str =
    let m = Regex.Match(str, "Operation: new = old (\*|\+) (new|old|[0-9]*)")
    if m.Success then
        fun i ->
            let operator = m.Groups[1].Value |> function | "+" -> (+) | "*" -> (*) | _ -> failwith "not an operator"
            let operand = m.Groups[2].Value |> function | "old" -> i | num -> int64 num
            (operator i operand) |> reducer
    else failwith "not starting items"

let test rule ifTrue ifFalse =
    let m1 = Regex.Match(rule, "Test: divisible by ([0-9]*)")
    let m2 = Regex.Match(ifTrue, "If true: throw to monkey ([0-9]*)")
    let m3 = Regex.Match(ifFalse, "If false: throw to monkey ([0-9]*)")
    if m1.Success && m2.Success && m3.Success then
        fun (i:int64) ->
            if i % (int64 m1.Groups[1].Value) = 0
            then (int m2.Groups[1].Value)
            else (int m3.Groups[1].Value)
    else failwith "not rule input"

let throwItem item receiver (monkeys: Map<int, Monkey>) =
    let receiversItems = monkeys[receiver].Items @ [item]
    monkeys |> Map.add receiver {monkeys[receiver] with Items = receiversItems}

let monkeyRound state monkeyNo monkey =
    let inspections = monkey.Items |> List.length |> int64
    monkey.Items
    |> List.fold (fun state' i ->
        let worryAfterInspection = monkey.Operation i
        let receiver = monkey.Test worryAfterInspection
        throwItem worryAfterInspection receiver state'
        )
        state
    |> Map.add monkeyNo {monkey with Inspections = monkey.Inspections + inspections; Items = []}

let round (monkeys: Map<int, Monkey>) =
    let mutable apa = monkeys
    monkeys
    |> Map.keys
    |> Seq.iter (fun i -> apa <- monkeyRound apa i apa[i] )
    apa

let parseInput reducer (input: string seq) =
    input
    |> Seq.split [[""]]
    |>> List.ofSeq
    |>> (fun (monkeySource: string list) ->
        {
            Items = startItems monkeySource[1] |> List.ofSeq
            Operation = operation reducer monkeySource[2]
            Test = test monkeySource[3] monkeySource[4] monkeySource[5]
            Inspections = 0
        }
    )
    |> Seq.mapi (fun i (x: Monkey) -> i,x)
    |> Map.ofSeq

let part1 =
    fileName
    >>= readFile
    |>> parseInput (fun i -> i / 3L)
    |>> (fun monkeys ->
            seq{1..20} |> Seq.fold (fun state i -> round state) monkeys
        )
    |>> Map.toList
    |>> (List.map (fun (i, (m: Monkey)) -> m.Inspections))
    |>> List.sortDescending
    |>> (fun (l: int64 list) -> l[0] * l[1])
    |>> sprintf "Monkey business: %i"

let part2 =
    fileName
    >>= readFile
    |>> parseInput (fun i -> i % primeproduct)
    |>> (fun monkeys ->
            seq{1..10000}
            |> Seq.fold (fun state i -> round state) monkeys
        )
    |>> Map.toList
    |>> (List.map (fun (i, (m: Monkey)) -> m.Inspections))
    |>> List.sortDescending
    |>> (fun (l: int64 list) -> l[0] * l[1])
    |>> sprintf "Monkey business: %i"

module Tests =
    let private tests = 
        [
            fun () ->
                let monkeys = [
                    0, {Items = [10L]; Operation = id ; Test = (fun _ -> 1); Inspections = 0}
                    1, {Items = []; Operation = id ; Test = (fun _ -> 0); Inspections = 0}
                ]
                monkeyRound (Map.ofList monkeys) 0 (snd monkeys[0])
                |> Map.toList
                |> function
                | [
                    0, {Items = []; Operation = _ ; Test = _; Inspections = 1L}
                    1, {Items = [10L]; Operation = _ ; Test = _ ; Inspections = 0L}
                    ] -> Ok ()
                | other -> Error $"Monkeyround failed, was {other}"
            fun () ->
                let monkeys = [
                    0, {Items = []; Operation = id ; Test = (fun _ -> 1); Inspections = 1L}
                    1, {Items = [10L]; Operation = (fun x -> x + 1L) ; Test = (fun _ -> 0); Inspections = 0L}
                ]
                monkeyRound (Map.ofList monkeys) 1 (snd monkeys[1])
                |> Map.toList
                |> function
                | [
                    0, {Items = [11L]; Operation = _ ; Test = _; Inspections = 1L}
                    1, {Items = []; Operation = _ ; Test = _ ; Inspections = 1L}
                    ] -> Ok ()
                | other -> Error $"Monkeyround failed, was {other}"

            fun () ->
                let monkeys = [
                    0, {Items = [primeproduct - 1L]; Operation = (fun i -> (i + 3L) % primeproduct ) ; Test = (fun _ -> 1); Inspections = 0}
                    1, {Items = []; Operation = id ; Test = (fun _ -> 0); Inspections = 0}
                ]
                monkeyRound (Map.ofList monkeys) 0 (snd monkeys[0])
                |> Map.toList
                |> function
                | [
                    0, {Items = []; Operation = _ ; Test = _; Inspections = 1L}
                    1, {Items = [2L]; Operation = _ ; Test = _ ; Inspections = 0L}
                    ] -> Ok ()
                | other -> Error $"Monkeyround mod failed, was {other}"

            fun () ->
               [
                0, {Items = [10L]; Operation = id ; Test = (fun _ -> 1); Inspections = 0}
                1, {Items = []; Operation = id ; Test = (fun _ -> 0); Inspections = 0}
               ] |> Map.ofList
               |> round |> Map.toList
               |> function
                | [
                    0, {Items = [10L]; Operation = _ ; Test = _; Inspections = 1L}
                    1, {Items = []; Operation = _ ; Test = _ ; Inspections = 1L}
                   ] -> Ok ()
                | other -> Error $"round failed, was {other}"
        ]
    let run () =
        tests 
        |> List.fold (fun state test -> state |> Result.bind test ) (Ok ())
        |>> fun () -> "All tests Ok"

Result.map3
    (sprintf "Successful run!\r\nTests: %s\r\nPart1: %s\r\nPart2: %s")
    (Tests.run ())
    part1
    part2
|> function
| Ok s -> s
| Error s -> sprintf "Error: %s" s 
|> Console.WriteLine 
