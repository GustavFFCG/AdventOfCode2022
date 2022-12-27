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

type FlatInput = {
    Map: Map<(int*int), bool>
    Path: Instruction list
}

type CubeInput = {
    Map: Map<(int*int), (bool* int)>
    Side: int
    Path: Instruction list
}

type State = {
    Position: int * int
    Facing:  Direction
}

let parseFlatInput (strings: string seq) : FlatInput =
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

let toCubeInput (flatInput: FlatInput) : CubeInput =
    let side = ((flatInput.Map.Keys |> Seq.map fst |> Seq.max) + 1) / 3
    {
        Path = flatInput.Path
        Side = side
        Map = 
            flatInput.Map
            |> Map.map (fun (x, y) dot -> 
            dot,
                if y < side then
                    if x < side * 2 then 1 else 2
                elif y < side * 2 then 3
                elif y < side * 3 then
                    if x < side then 4 else 5
                else 6
            )
    }

let moveFlat (map:Map<(int*int), bool>) instruction (state:State) =
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
let moveCube (map:Map<(int*int), (bool*int)>) (side: int) instruction (state:State) =
    Console.WriteLine $"Moving on cube with side {side} from {state} by {instruction}"
    match instruction with
    | Right -> { 
            Position = state.Position
            Facing = 
                state.Facing 
                |> function | North -> East | East -> South | South -> West | West -> North 
        }
    | Left -> {
            Position = state.Position
            Facing = 
                state.Facing 
                |> function | North -> West | West -> South | South -> East | East -> North
        }
    | Step i when i <= 0 -> state
    | Step steps ->
        {1..steps}
        |> Seq.fold
            (fun state _ ->
                let x, y = fst state.Position, snd state.Position
                let nextPos, facing = 
                    match state.Facing with
                    | North -> (x, y - 1)
                    | South -> (x, y + 1)
                    | East -> (x + 1, y)
                    | West -> (x - 1, y)
                    |> fun pos' -> 
                        if Map.containsKey pos' map && (snd map[pos']) = ((snd map[(x,y)])) then pos', state.Facing
                        else
                            match state.Facing, snd map[(x,y)] with
                            | North, 1 -> 
                                    (0, side * 3 + x % side), West
                            | South, 1 ->
                                pos', South
                            | East, 1 ->
                                pos', East
                            | West, 1 ->
                                (0, side * 3 - 1 - y % side), East

                            | North, 2 ->
                                (x % side, side * 4 - 1), North
                            | South, 2 ->
                                (side * 2 - 1, side + x % side), West
                            | East, 2 ->
                                (side * 2 - 1, side * 3 - 1 - y), West
                            | West, 2 ->
                                pos', West

                            | North, 3 -> 
                                pos', North
                            | South, 3 ->
                                pos', South
                            | East, 3 ->
                                (side * 2 + y % side, side - 1), North
                            | West, 3 ->
                                (y % side, side * 2), South

                            | North, 4 -> 
                                (side, side + x), East
                            | South, 4 ->
                                pos', South
                            | East, 4 ->
                                pos', East
                            | West, 4 ->
                                (side, side - 1 - y % side), East

                            | North, 5 -> 
                                pos', North
                            | South, 5 ->
                                (side - 1, side * 3  + x % side), North
                            | East, 5 ->
                                (side * 3 - 1, side - 1 - y % side), West
                            | West, 5 ->
                                pos', West

                            | North, 6 -> 
                                pos', North
                            | South, 6 ->
                                (side * 2 + x, 0), South
                            | East, 6 ->
                                (side + y % side, side * 3 - 1), North
                            | West, 6 ->
                                (side + y % side, 0), South

                            | _ -> failwith "illegal position"
                if not (Map.containsKey nextPos map) then 
                    failwith $"Could not go to {nextPos} facing {facing} at ({x}, {y}) (Side {snd map[(x, y)]})"
                elif fst map[nextPos] then { Position = nextPos ; Facing = facing }
                else state
            )
            state

let password (state: State) =
    Console.WriteLine $"Password for state {state}"
    (state.Facing |> function | East -> 0L | South -> 1L | West -> 2L | North -> 3L)
    + 1000L * int64 ((snd state.Position) + 1)
    + 4L * int64 ((fst state.Position) + 1)

let part1 input =
    let notes = input |> parseFlatInput
    let initialState = {
        Position = notes.Map |> Map.keys |> Seq.where (fun (x,y) -> y = 0) |> Seq.min
        Facing = East
    }
    notes.Path
    |> List.fold (fun state instruction -> moveFlat notes.Map instruction state) initialState
    |> password
    |> sprintf "Password is %i"

let part2 input = 
    let notes = input |> parseFlatInput |> toCubeInput
    let initialState = {
        Position = notes.Side * 2, 0
        Facing = East
    }
    notes.Path
    |> List.fold (fun state instruction -> moveCube notes.Map notes.Side instruction state) initialState
    |> password
    |> sprintf "Password is %i"

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
