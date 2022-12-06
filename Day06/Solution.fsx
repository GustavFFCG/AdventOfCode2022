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

let findMarker (s: string) =
    s
    |> Seq.mapi (fun i c ->
        if i < 4 then None
        else
            s[(i - 4)..i]
            |> Set.ofSeq
            |> Set.count
            |> function | 4 -> None | _ -> Some i

        )
    |> Seq.find Option.isSome
    |> fun x -> x
    |> Option.defaultValue 0
    |> fun x -> x + 1

module Tests =
    let test (s, i) =
        let result = findMarker s
        if result = i then Ok () 
        else Error $"{s} should give {i}, was {result}"

    let private tests = 
        [
            fun () -> test ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7) 
            fun () -> test ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5) 
        ]
    let run () =
        tests 
        |> List.fold (fun state test -> state |> Result.bind test ) (Ok ())
        |>> fun () -> "All tests Ok"

let part1 = Ok "todo"

let part2 = Ok "todo"

Result.map3
    (sprintf "Successful run!\r\nTests: %s\r\nPart1: %s\r\nPart2: %s")
    (Tests.run ())
    part1
    part2
|> function
| Ok s -> s
| Error s -> sprintf "Error: %s" s 
|> Console.WriteLine 
