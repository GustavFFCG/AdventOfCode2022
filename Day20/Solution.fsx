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
    if value >= length then (value % length) + 1
    elif value < 0 then length + (value % length) - 1
    elif value = 0 then length - 1
    else value

let mixOne (state: (int*bool) list) = 
    let presentPlace = state |> List.findIndex ( snd >> not )
    let itemToMove = state[presentPlace]
    let newPlace = place (List.length state) (presentPlace + (fst itemToMove))
    Console.WriteLine $"Moving {presentPlace} ({itemToMove}) to {newPlace}"
    if newPlace > presentPlace then //1 -> 3
        (List.take presentPlace state) // 0
        @ state[presentPlace + 1 .. newPlace] //..23
        @ [(fst itemToMove, true)] // 1
        @ ((List.skip (newPlace + 1) state)) //....4 
    elif newPlace < presentPlace then // 3 -> 1
        let head = (List.take newPlace state)
        let mid = state[newPlace .. presentPlace - 1]
        let item = [(fst itemToMove, true)]
        let tail = ((List.skip (presentPlace) state))
        Console.WriteLine $"{head} @ {item} @ {mid} @  {tail}"
        head @ item @ mid @ tail
        //(List.take newPlace state) // 0
        //@ state[newPlace + 1 .. presentPlace] //..23
        //@ [(fst itemToMove, true)] // 1
        //@ ((List.skip (presentPlace + 1) state)) //....4
    else
        state 
        |> List.mapi (fun pos (item, moved) -> 
            if pos = presentPlace then  (item, true) else (item, moved))

let mix (original: int list) =
    let unmixed = original |>> fun i -> (i, false)
    {0..List.length original}
    |> Seq.fold
        (fun state i ->
            Console.Write $"Before step {i}: "
            state |> List.iter (fun (i, moved) -> (if moved then $"({i}) " else $" {i}  ") |> Console.Write)
            Console.Write "\r\n"
            mixOne state
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
                    (4, 0, 3)
                    (4, 1, 1)
                    (4, 4, 1)
                    (4, 5, 2)
                    (4, -1, 2)
                ]
                |> List.fold
                    (fun state (length, value, expected) -> 
                        state >>= (fun () -> place length value |> fun actual -> 
                            if actual = expected then Ok () else Error $"place {length} {value}: Expected {expected}, was {actual}"))
                    (Ok ())
            fun () -> 
                [
                    ([(1, false);(0, false)], [(0, false);(1, true)])
                    ([(-1, false);(0, false);(2,false)], [(0, false);(-1, true);(2, false)])
                    ([(1, true);(-3, false);(2, true);(3, false);(-2, false);(0, false);(4, false)], 
                    [(1, true);(2,true);(3,false);(-2,false);(-3,true);(0,false);(4, false)])
                ]
                |> List.fold
                    (fun state (given, expected) -> 
                        state >>= (fun () -> mixOne given |> fun actual -> 
                            if actual = expected then Ok () else Error $"mixOne {given}: Expected {expected}, was {actual}"))
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
