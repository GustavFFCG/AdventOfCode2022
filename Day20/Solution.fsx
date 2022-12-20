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

let readFile fileName =
    try
        File.ReadLines fileName
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let mix (original: int list) =
    let unmixed = original |> fun i -> (i, false)
    {0..List.length original}
    |> Seq.fold
        (fun state i ->
            let presentPlace = state |> List.findIndex ( snd >> not )
            let itemToMove = state[presentPlace]
            let newPlace = (presentPlace + (fst itemToMove)) % (List.length original)
            (List.take newPlace state) @ [(fst itemToMove, false)] @ (List.skip (newPlace + 1) state)
            )
        unmixed
 

let part1 input =
    "todo"

let part2 input = 
    "todo"

module Tests =
    let private tests = 
        [
            fun () -> 
                [1; 2; -3; 3; -2; 0; 4] |> mix 
                |> function | [1; 2; -3; 4; 0; 3; -2 ] -> Ok () | other -> Error "Error mixing sample"
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
