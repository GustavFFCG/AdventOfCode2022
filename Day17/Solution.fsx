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
    | _::s::_ -> Some s 
    | _ -> None

type Jetstream = | Left | Right
    with static member ofChar = function | '<' -> Left | '>' -> Right | _ -> failwith "Unexpected char input"

let readFile fileName =
    try
        File.ReadLines fileName
        |> Seq.head
        |> Seq.map Jetstream.ofChar
        |> Array.ofSeq
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let shapes = 
    [|
        [|(0,0);(1,0);(2,0);(3,0)|] // -
        [|(1,0);(0,1);(1,1);(2,1);(1,2)|] // +
        [|(0,0);(1,0);(2,0);(2,1);(2,2)|] // _|
        [|(0,0);(0,1);(0,2);(0,3)|] // |
        [|(0,0);(0,1);(1,0);(1,1)|] // ¤
    |] |> (Array.map (Set.ofArray))

let tetris rounds (jetStream:Jetstream[]) (shapes: Set<int*int>[]) =
    let mutable streamPos = 0
    let mutable well = Set.empty
    let mutable startHeight = 3
    let canFall shape =
        shape |> Seq.exists (fun (x, y) -> y = 0 || Set.contains (x, y-1) well) |> not

    let tryMoveLeft shape =
        if Set.exists (fun (x, y) -> x = 0  || Set.contains (x-1, y) well) shape
        then None 
        else shape |> Set.map (fun (x, y) -> (x-1, y)) |> Some

    let tryMoveRight shape =
        if Set.exists (fun (x, y) -> x = 6  || Set.contains (x+1, y) well) shape
        then None 
        else shape |> Set.map (fun (x, y) -> (x+1, y)) |> Some

    let tryMoveDown shape =
        if Set.exists (fun (x, y) -> y = 0  || Set.contains (x, y - 1) well) shape
        then None 
        else shape |> Set.map (fun (x, y) -> (x, y-1)) |> Some

    {1..rounds}
    |> Seq.iter ( fun i ->
        let mutable fallingShape = shapes[(i-1) % shapes.Length] |> Set.map(fun (x, y) -> (x + 2, y + startHeight))

        fallingShape
        |> match jetStream[streamPos % Array.length jetStream] with
            | Left -> tryMoveLeft
            | Right -> tryMoveRight
        |> function
        | Some shape -> fallingShape <- shape
        | None -> ()
        streamPos <- streamPos + 1

        while canFall fallingShape do
            match tryMoveDown fallingShape with
                | Some shape -> fallingShape <- shape
                | None -> ()

            fallingShape
            |> match jetStream[streamPos % Array.length jetStream] with
                | Left -> tryMoveLeft
                | Right -> tryMoveRight
            |> function
            | Some shape -> fallingShape <- shape
            | None -> ()
            streamPos <- streamPos + 1

        well <- Set.union fallingShape well
        let height = well |> (Seq.maxBy (fun (_x, y) -> y)) |> (snd >> (+) 1)
        startHeight <- height + 3
    )
    //{0..20}
    //|> Seq.iter (fun y ->
    //    {0..6} |> Seq.iter (fun x ->
    //        if Set.contains (x,20 - y) well then Console.Write "#" else Console.Write "."
    //    )
    //    Console.Write "\r\n"
    //)
    startHeight - 3

let part1 input =
    tetris 2022 input shapes
    |> sprintf "stacked to %i"

let part2 input = 
    "todo"

module Tests =
    let private tests = 
        [
            fun () ->
                tetris 1 ([|'<'|] |>> Jetstream.ofChar) ([| [|(0,0)|] |] |> (Array.map (Set.ofArray)))
                |> function | 1 -> Ok () | i -> Error $"One pebble should be 1, was {i}"
            fun () ->
                tetris 2 (">>><<<" |> Array.ofSeq |>> Jetstream.ofChar) ([| [|(0,0)|] |] |> (Array.map (Set.ofArray)))
                |> function | 1 -> Ok () | i -> Error $"Two pebbles in different winds should be 1, was {i}"
            fun () ->
                tetris 2
                    (">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" |> Array.ofSeq |>> Jetstream.ofChar)
                    shapes
                |> function | 4 -> Ok () | i -> Error $"9 Should be 4, was {i}"
            fun () ->
                tetris 10
                    (">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" |> Array.ofSeq |>> Jetstream.ofChar)
                    shapes
                |> function | 17 -> Ok () | i -> Error $"Should be 17, was {i}"
            fun () ->
                tetris 2022 
                    (">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" |> Array.ofSeq |>> Jetstream.ofChar)
                    shapes
                |> function | 3068 -> Ok () | i -> Error $"Sample should be 3068, was {i}"
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
