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

module Rucksack =
    let ofString (s: string) =
        let midpoint = s.Length /2
        s.[0..(midpoint-1)], s.[midpoint .. s.Length]

    let findError ((a: char seq),(b: char seq)) =
        a |> Seq.find (fun c -> b |> Seq.contains c)

    let itemPriority (c: char) =
        if Char.IsLower c then
            int c - int 'a' + 1
        else
            int c - int 'A' + 27

    let findBadge (a: string) (b: string) (c: string) =
        a |> Seq.find (fun x -> b |> Seq.contains x && c |> Seq.contains x )

let part1 = 
    fileName
    >>= readFile
    |>> Seq.map Rucksack.ofString
    |>> Seq.map Rucksack.findError
    |>> Seq.map Rucksack.itemPriority
    |>> Seq.sum
    |>> sprintf "Sum is %i"

let part2 = 
    fileName
    >>= readFile
    |>> Seq.chunkBySize 3
    |>> Seq.map (fun (arr: string[]) -> Rucksack.findBadge arr[0] arr[1] arr[2])
    |>> Seq.map Rucksack.itemPriority
    |>> Seq.sum
    |>> sprintf "Sum is %i"
            
module Tests =
    let private tests = 
        [
            fun () -> 
                "vJrwpWtwJgWrhcsFMMfFFhFp"
                |> Rucksack.ofString
                |> function
                | "vJrwpWtwJgWr", "hcsFMMfFFhFp" -> Ok ()
                | e -> Error $"Wrong split: {e}"
            fun () -> 
                ("vJrwpWtwJgWr", "hcsFMMfFFhFp")
                |> Rucksack.findError
                |> function
                | 'p' -> Ok ()
                | c -> Error $"Wrong found diff: {c}"

            fun () -> 
                ['b' ; 'B'] |>> Rucksack.itemPriority
                |> function
                | [2; 28] -> Ok ()
                | e -> Error $"Wrong priority: {e}"

            fun () -> 
                Rucksack.findBadge "vJrwpWtwJgWrhcsFMMfFFhFp" "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" "PmmdzqPrVvPwwTWBwg"
                |> function
                | 'r' -> Ok ()
                | c -> Error $"Wrong found diff: {c}"

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
