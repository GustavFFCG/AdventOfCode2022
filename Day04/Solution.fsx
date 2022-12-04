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

let assignmentPair s =
    s |> String.split [| "-"; ","|] |>> int
    |> List.ofSeq |> function
        | [a;b;c;d] -> (Set.ofSeq {a..b}, Set.ofSeq {c..d})
        | _ -> failwith "unexpected input"

let anyIsSubset s1 s2 =
    Set.isSubset s1 s2 || Set.isSubset s2 s1

let anyOverlap s1 s2 =
    Set.intersect s1 s2 <> Set.empty

module Tests =
    let private tests = 
        [
            //fun () -> Error "todo"
        ]
    let run () =
        tests 
        |> List.fold (fun state test -> state |> Result.bind test ) (Ok ())
        |>> fun () -> "All tests Ok"

let part1 =
    fileName
    >>= readFile
    |>> map assignmentPair
    |>> map (fun (s1, s2)  -> anyIsSubset s1 s2)
    |>> Seq.where id
    |>> Seq.length
    |>> sprintf "There are %i pairs"

let part2 =
    fileName
    >>= readFile
    |>> map assignmentPair
    |>> map (fun (s1, s2)  -> anyOverlap s1 s2)
    |>> Seq.where id
    |>> Seq.length
    |>> sprintf "There are %i overlaps"

Result.map3
    (sprintf "Successful run!\r\nTests: %s\r\nPart1: %s\r\nPart2: %s")
    (Tests.run ())
    part1
    part2
|> function
| Ok s -> s
| Error s -> sprintf "Error: %s" s 
|> Console.WriteLine 
