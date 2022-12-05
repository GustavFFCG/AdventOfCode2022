#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus

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

type Move = {
    Amount: int
    Source: int
    Destination: int
}
type Input = {
    Stacks: Map<int, char list>
    Moves: Move seq
}
let parseInput (input: string list) =
    let parseStart (start: string list) =
        start
        |> List.rev
        |> function
        | head::tail ->
            let positions =
                head
                |> Seq.mapi (fun i c -> 
                    match c with | ' ' -> None | c' -> Some ((int c' - int '0'), i)
                    )
                |> Seq.choose id
                |> Map.ofSeq

            positions
            |> Map.mapValues (fun index ->
                tail
                |> List.fold
                    (fun state s ->
                        match s[index] with | ' ' -> state | c -> c::state)
                        [])
        | other -> failwith $"unexpected input {other}"

    let parseMove (move: string) : Move =
        move
        |> String.split [|"move "; " from "; " to"|]
        |> List.ofSeq
        |> List.tail
        |>> int
        |> function
            | ([number; source; dest]:int list) -> { Amount = number; Source = source; Destination = dest}
            | other -> failwith $"unexpected input {other}"
    let start =
        input |> List.takeWhile (fun s -> String.IsNullOrWhiteSpace(s) |> not)
    let moves =
        input 
        |> List.where (String.startsWith "move")
    {
        Stacks = parseStart start
        Moves = moves |>> parseMove 
    }

let doMove (stacks:Map<int, char list>) (move:Move) : Map<int, char list> =
    let sourceStack = stacks[move.Source] |> List.skip move.Amount
    let load = stacks[move.Source] |> List.take move.Amount |> List.rev
    let destinationStack = load @ stacks[move.Destination]
    stacks |> Map.add move.Source sourceStack |> Map.add move.Destination destinationStack

let doMove9001 (stacks:Map<int, char list>) (move:Move) : Map<int, char list> =
    let sourceStack = stacks[move.Source] |> List.skip move.Amount
    let load = stacks[move.Source] |> List.take move.Amount
    let destinationStack = load @ stacks[move.Destination]
    stacks |> Map.add move.Source sourceStack |> Map.add move.Destination destinationStack

let charsToString (chars: Char seq) = String.Concat chars

let part1 =
    fileName
    >>= readFile
    |>> List.ofSeq
    |>> parseInput
    |>> (fun input ->
        input.Moves
        |> Seq.fold 
            doMove
            input.Stacks)
    |>> (Map.mapValues head)
    |>> Map.values
    |>> charsToString

let part2 =
    fileName
    >>= readFile
    |>> List.ofSeq
    |>> parseInput
    |>> (fun input ->
        input.Moves
        |> Seq.fold 
            doMove9001
            input.Stacks)
    |>> (Map.mapValues head)
    |>> Map.values
    |>> charsToString

module Tests =
    let sample = [
        "    [D]        "
        "[N] [C]        "
        "[Z] [M] [P]    "
        " 1   2   3     "
        " "
        "move 1 from 2 to 1"
        "move 3 from 1 to 3"
        "move 2 from 2 to 1"
        "move 1 from 1 to 2"
    ]
    let private tests = 
        [
            fun () -> 
                let stacks = 
                    [
                        1, ['N';'Z']
                        2, ['D';'C';'M']
                        3, ['P']
                    ]
                    |> Map.ofList
                let move = {Amount = 1; Source = 2; Destination = 1}
                doMove stacks move
                |> Map.toList
                |> function
                    | 
                        [
                            1, ['D';'N';'Z']
                            2, ['C';'M']
                            3, ['P']
                        ] -> Ok ()
                    | other -> Error $"{other}" 
            fun () -> 
                let stacks = 
                    [
                        1, ['D';'N';'Z']
                        2, ['C';'M']
                        3, ['P']
                    ]
                    |> Map.ofList
                let move = {Amount = 3; Source = 1; Destination = 3}
                doMove stacks move
                |> Map.toList
                |> function
                    | 
                        [
                            1, []
                            2, ['C';'M']
                            3, ['Z';'N';'D';'P']
                        ] -> Ok ()
                    | other -> Error $"{other}" 
        ]
    let run () =
        tests 
        |> List.fold (fun state test -> state |> Result.bind test ) (Ok ())
        |>> fun () -> $"All %i{tests.Length} tests Ok"


Result.map3
    (sprintf "Successful run!\r\nTests: %s\r\nPart1: %s\r\nPart2: %s")
    (Tests.run ())
    part1
    part2
|> function
| Ok s -> s
| Error s -> sprintf "Error: %s" s 
|> Console.WriteLine 
