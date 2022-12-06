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
        |> Seq.tryHead
        |> function | Some s -> Ok s | None -> Error "unexpected input"
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let findMarker length (s: string) =
    s
    |> Seq.mapi (fun i c ->
        if i < length then None
        else
            s[(i - length)..(i - 1)]
            |> Set.ofSeq
            |> Set.count
            |> function | l when l = length -> Some i | _ -> None
        )
    |> Seq.find Option.isSome
    |> function | Some i -> Ok i | None -> Error "No marker found"

let part1 =
    fileName
    >>= readFile
    >>= findMarker 4
    |>> sprintf "%i"

let part2 =
    fileName
    >>= readFile
    >>= findMarker 14
    |>> sprintf "%i"


module Tests =
    let testPart1 s i =
        let result = findMarker 4 s
        if result = i then Ok () 
        else Error $"{s} should give {i}, was {result}"

    let testPart2 s i =
        let result = findMarker 14 s
        if result = i then Ok () 
        else Error $"{s} should give {i}, was {result}"

    let private tests = 
        [
            fun () -> testPart1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" (Ok 7)
            fun () -> testPart1 "bvwbjplbgvbhsrlpgdmjqwftvncz" (Ok 5)
            fun () -> testPart2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" (Ok 19)
            fun () -> testPart2 "bvwbjplbgvbhsrlpgdmjqwftvncz" (Ok 23)
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
