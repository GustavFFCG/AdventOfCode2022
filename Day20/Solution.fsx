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

let place length value =
    Console.WriteLine $"Placing {value} of {length}"
    value % (length) |> fun x -> if x >= 0 then x else length + x

let mix (original: int list) =
    let unmixed = original |>> fun i -> (i, false)
    {0..List.length original}
    |> Seq.fold
        (fun state i ->
            Console.Write $"Before step {i}: "
            state |> List.iter (fun (i, moved) -> (if moved then $"({i}) " else $" {i}  ") |> Console.Write)
            Console.Write "\r\n"
            let presentPlace = state |> List.findIndex ( snd >> not )
            let itemToMove = state[presentPlace]
            let newPlace = place (presentPlace + (fst itemToMove)) (List.length original)
            Console.WriteLine $"Moving {presentPlace} ({itemToMove}) to {newPlace}"
            if newPlace > presentPlace then
                (List.take presentPlace state)
                @ state[presentPlace + 1 .. newPlace]
                @ [(fst itemToMove, true)]
                @ ((List.skip (newPlace + 1) state))
            elif newPlace < presentPlace then
                (List.take newPlace state)
                @ state[newPlace + 1 .. presentPlace]
                @ [(fst itemToMove, true)]
                @ ((List.skip (presentPlace + 1) state))
            else 
                (List.take presentPlace state)
                @ [(fst itemToMove, true)]
                @ ((List.skip (presentPlace + 1) state))
        )
        unmixed
    |>> fst
 

let part1 input =
    "todo"

let part2 input = 
    "todo"

module Tests =
    let private tests = 
        [
            fun () -> 
                [
                    (4, 0, 0)
                    (4, 1, 1)
                    (4, 4, 0)
                    (4, 5, 1)
                    (4, -1, 3)
                ]
                |> List.fold
                    (fun state (length, value, expected) -> 
                        state >>= (fun () -> place length value |> fun actual -> 
                            if actual = expected then Ok () else Error $"place {length} {value}: Expected {expected}, was {actual}"))
                    (Ok ())
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
