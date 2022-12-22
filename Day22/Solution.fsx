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

type Instruction =
    | Right
    | Left
    | Step of int

type Direction =  North | South | East | West

type Input = {
    Map: Map<(int*int), bool>
    Path: Instruction list
}

type State = {
    Position: int * int
    Facing:  Direction
}

let parseInput (strings: string seq)=
    let parseMap (map: string seq) =
        map
        |> Seq.mapi
            (fun y str ->
                str
                |> Seq.mapi 
                    (fun x c ->
                        match c with
                        | '.' -> Some ((x, y),true)
                        | '#' -> Some ((x, y),false)
                        | _ -> None
                    )
                |> Seq.choose id
            )
        |> Seq.concat
        |> Map.ofSeq

    let parsePath path =
        let rec parse str =
            if str = "" then []
            else
                let stepMatch = Regex.Match(str, "^([0-9]+)(.*)")
                let turnMatch = Regex.Match(str, "^(R|L)(.*)")
                if stepMatch.Success then (Step (int stepMatch.Groups[1].Value))::parse stepMatch.Groups[2].Value
                elif turnMatch.Success then 
                    (turnMatch.Groups[1].Value |> function "R" -> Right | "L" -> Left | _ -> failwith "unexpected turn")
                    ::parse turnMatch.Groups[2].Value
                else failwith "unexpected direction"
        parse path

    strings |> Seq.split [| [|""|]|] |> List.ofSeq
    |> function
    | [map;path] -> {Map = parseMap map; Path = path |> head |> parsePath}
    | _ -> failwith "unexpected input"

let move (map:Map<(int*int), bool>) instruction (state:State) =
    //Console.WriteLine $"Moving from {state} by {instruction}"
    let facing = 
        match instruction with
        | Step _ -> state.Facing
        | Right -> state.Facing |> function | North -> East | East -> South | South -> West | West -> North
        | Left -> state.Facing |> function | North -> West | West -> South | South -> East | East -> North
    let position = 
        match instruction with
        | Right -> state.Position
        | Left -> state.Position
        | Step i when i <= 0 -> state.Position
        | Step steps ->
            {1..steps}
            |> Seq.fold
                (fun (x,y) _ ->
                    let nextPos = 
                        match state.Facing with
                        | North -> (x, y - 1)
                        | South -> (x, y + 1)
                        | East -> (x + 1, y)
                        | West -> (x - 1, y)
                        |> fun pos' -> 
                            if Map.containsKey pos' map then pos'
                            else 
                                match state.Facing with
                                | North -> 
                                        map |> Map.keys |> Seq.where (fun (x', _) -> x' = x) |> Seq.max
                                | South ->
                                        map |> Map.keys |> Seq.where (fun (x', _) -> x' = x) |> Seq.min
                                | East ->
                                        map |> Map.keys |> Seq.where (fun (_, y') -> y' = y) |> Seq.min
                                | West ->
                                        map |> Map.keys |> Seq.where (fun (_, y') -> y' = y) |> Seq.max
                    if map[nextPos] then nextPos
                    else (x,y)
                )
                state.Position
    {Position = position; Facing = facing}

let password (state: State) =
    Console.WriteLine $"Password for state {state}"
    (state.Facing |> function | East -> 0L | South -> 1L | West -> 2L | North -> 3L)
    + 1000L * int64 ((snd state.Position) + 1)
    + 4L * int64 ((fst state.Position) + 1)

let part1 input =
    let notes = input |> parseInput
    let initialState = {
        Position = notes.Map |> Map.keys |> Seq.where (fun (x,y) -> y = 0) |> Seq.min
        Facing = East
    }
    notes.Path
    |> List.fold (fun state instruction -> move notes.Map instruction state) initialState
    |> password
    |> sprintf "Password is %i"

let part2 input = 
    "todo"

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
