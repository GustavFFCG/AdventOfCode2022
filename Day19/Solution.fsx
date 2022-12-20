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

type Material = 
| Ore
| Clay
| Obsidian
| Geode

type Blueprint = {
    OreCost: {|Ore:int|}
    ClayCost: {|Ore:int|}
    ObsidianCost: {|Ore:int; Clay:int|}
    GeodeCost: {|Ore:int; Obsidian:int|}
}

let parseInput input =
    input
    |> Seq.fold 
        (fun state s ->
            let pattern = "Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore\. Each clay robot costs ([0-9]+) ore\. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay\. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian\."
            let m = Regex.Match(s, pattern)
            if m.Success then
                Map.add
                    m.Groups[1].Value
                    {
                        OreCost = {| Ore= int m.Groups[2].Value |}
                        ClayCost = {| Ore = int m.Groups[3].Value |}
                        ObsidianCost = {| Ore = int m.Groups[4].Value; Clay = int m.Groups[5].Value |}
                        GeodeCost = {| Ore = int m.Groups[6].Value ; Obsidian = int m.Groups[7].Value |}
                    }
                    state
            else failwith $"Unexpected input: {s}"
        )
        Map.empty

let optimizeBlueprint blueprint = "todo"

let part1 input =
    input
    |> parseInput
    |> Map.count
    |> sprintf "%i blueprints"

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
