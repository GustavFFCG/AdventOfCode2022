#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus
open System.Text.RegularExpressions
open System.Collections.Generic

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

type TypeCount = {
    Ore: int
    Clay: int
    Obsidian: int
    Geode: int
}
with 
    static member (+) (tc1,tc2) = 
        {
            Ore = tc1.Ore + tc2.Ore
            Clay = tc1.Clay + tc2.Clay
            Obsidian = tc1.Obsidian + tc2.Obsidian
            Geode = tc1.Geode + tc2.Geode
        }
    static member (-) (tc1,tc2) = 
        {
            Ore = tc1.Ore - tc2.Ore
            Clay = tc1.Clay - tc2.Clay
            Obsidian = tc1.Obsidian - tc2.Obsidian
            Geode = tc1.Geode - tc2.Geode
        }
    static member (*) (tc,i) = 
        {
            Ore = tc.Ore * i
            Clay = tc.Clay * i
            Obsidian = tc.Obsidian * i
            Geode = tc.Geode * i
        }
    static member (/) (tc1,tc2) = 
        [|
            if tc2.Ore = 0      then None else Some (tc1.Ore / tc2.Ore)
            if tc2.Clay = 0     then None else Some (tc1.Clay / tc2.Clay)
            if tc2.Obsidian = 0 then None else Some (tc1.Obsidian / tc2.Obsidian)
            if tc2.Geode = 0    then None else Some (tc1.Geode / tc2.Geode)
        |] |> Seq.choose id |> Seq.min
    static member op_GreaterThanOrEqual (tc1,tc2) = 
            tc1.Ore >= tc2.Ore
            && tc1.Clay >= tc2.Clay
            && tc1.Obsidian >= tc2.Obsidian
            && tc1.Geode >= tc2.Geode
    static member free =
        {
            Ore = 0
            Clay = 0
            Obsidian = 0
            Geode = 0
        }

type Blueprint = {
    OreCost: TypeCount
    ClayCost: TypeCount
    ObsidianCost: TypeCount
    GeodeCost: TypeCount
}


type State = {
    Robots: TypeCount
    Resources: TypeCount
}

let parseInput input =
    input
    |> Seq.split [| [|""|] |]
    |>> (fun lines -> String.Join(Environment.NewLine, lines))
    |> Seq.fold 
        (fun state s ->
            let pattern = "Blueprint ([0-9]+):(?:.|\n|\r)*Each ore robot costs ([0-9]+) ore\.(?:.|\n|\r)*Each clay robot costs ([0-9]+) ore\.(?:.|\n|\r)*Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay\.(?:.|\n|\r)*Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian\."
            let m = Regex.Match(s, pattern)
            if m.Success then
                (m.Groups[1].Value,
                {
                    OreCost =       { TypeCount.free with Ore = int m.Groups[2].Value }
                    ClayCost =      { TypeCount.free with Ore = int m.Groups[3].Value }
                    ObsidianCost =  { TypeCount.free with Ore = int m.Groups[4].Value; Clay = int m.Groups[5].Value }
                    GeodeCost =     { TypeCount.free with Ore = int m.Groups[6].Value; Obsidian = int m.Groups[7].Value }
                })
                ::state
            else failwith $"Unexpected input: {s}"
        )
        []

let maxGeodesFromBlueprint (blueprint:Blueprint) =
    let robotConstruction (state: State) =
        let evalStep cost robotFun resourceFun prevState =
            let newState = new List<State>()
            newState.AddRange prevState
            for state' in prevState do
                if state'.Resources >= cost
                then {1..state'.Resources / cost}
                else Seq.empty
                |> Seq.iter ( fun i ->
                    newState.Add 
                        {state' with 
                            Robots = robotFun state'.Robots i
                            Resources = resourceFun state'.Resources i
                        }
                )
            newState
            
        let mutable newState = new HashSet<State>()
        newState.Add state |> ignore<bool>
        
        newState.UnionWith(
            evalStep
                blueprint.GeodeCost 
                (fun robots i -> {robots with Geode = robots.Geode + i})
                (fun resources i -> resources - (blueprint.GeodeCost * i))
                newState)
        newState.UnionWith(
            evalStep
                blueprint.ObsidianCost
                (fun robots i -> {robots with Obsidian = robots.Obsidian + i})
                (fun resources i -> resources - (blueprint.ObsidianCost * i))
                newState)
        newState.UnionWith(
            evalStep
                blueprint.ClayCost
                (fun robots i -> {robots with Clay = robots.Clay + i})
                (fun resources i -> resources - (blueprint.ClayCost * i))
                newState)
        newState.UnionWith(
            evalStep
                blueprint.OreCost
                (fun robots i -> {robots with Ore = robots.Ore + i})
                (fun resources i -> resources - (blueprint.OreCost * i))
                newState)

        newState.RemoveWhere(fun st -> 
            st.Resources >= blueprint.OreCost 
            || st.Resources >= blueprint.ClayCost
            || st.Resources >= blueprint.ObsidianCost
            || st.Resources >= blueprint.GeodeCost
            ) |> ignore<int>
        newState

    let mutable states = new HashSet<State>()
    states.Add(
            {
                Robots = {Ore = 1; Clay = 0; Obsidian = 0; Geode=0}
                Resources = {Ore = 0; Clay = 0; Obsidian = 0; Geode=0}
            }) |> ignore<bool>
    {1..24}
    |> Seq.iter (fun i ->
        Console.WriteLine $"Minute {i}, {states.Count} states..."
        let newState = new HashSet<State>()
        for state in states do
            for robotState in robotConstruction state do
                newState.Add({robotState with Resources = robotState.Resources + state.Robots}) |> ignore<bool>
        states <- newState
    )
    states 
    |> Seq.cast<State>
    |> Seq.map (fun s -> s.Resources.Geode)
    |> Seq.max

let part1 input =
    input
    |> parseInput
    |>> ((fun (s, b) ->
        Console.WriteLine $"Evaluating blueprint {s}"
        let geodes = maxGeodesFromBlueprint b
        Console.WriteLine $"Blueprint {s} gives {geodes} geodes"
        s, maxGeodesFromBlueprint b
        ) >> snd)
    |> List.max
    |> sprintf "%i geodes"

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
