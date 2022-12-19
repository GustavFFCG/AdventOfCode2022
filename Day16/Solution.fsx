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
type Valve = {
    FlowRate: int
    Tunnels: string array
}
let parseInput input =
    input
    |> Seq.fold 
        (fun state s ->
            let m = Regex.Match(s, "Valve ([A-Z]{2}) has flow rate=([0-9]+); tunnels? leads? to valves? (.*)")
            if m.Success then
                Map.add
                    m.Groups[1].Value
                    {
                        FlowRate = int m.Groups[2].Value
                        Tunnels = m.Groups[3].Value |> String.split [|", "|] |> Array.ofSeq
                    }
                    state
            else failwith $"Unexpected input: {s}"
        )
        Map.empty

let step (map: Map<string, Valve>) timeLeft (states:Map<(string*string Set), int>) =
    states
    |> Map.toSeq
    >>=  (fun ((pos,opened),flow) ->
        let currentValve = map[pos]
        currentValve.Tunnels
        |>> (fun nextPos -> ((nextPos,opened),flow))
        |> Seq.append 
            (
                if currentValve.FlowRate > 0 && not(Set.contains pos opened ) 
                then [((pos,Set.add pos opened), flow + currentValve.FlowRate * timeLeft )]
                else []
            )
    )
    |> Seq.fold 
        (fun (state:Map<(string*string Set), int>) ((pos,opened),flow) -> 
            if not (Map.containsKey (pos,opened) state) || state[(pos,opened)] < flow
            then Map.add (pos,opened) flow state else state
        )
        Map.empty

let stepWithKent (map: Map<string, Valve>) fullFlow timeLeft (states:byref<Dictionary<(string*string*(Set<string>)), int>>) =
    let mutable newStates = new Dictionary<(string*string*(Set<string>)), int>()
    for state in states do
        let (myPos,kentsPos,opened) = state.Key
        let currentFlow = state.Value
        if opened = fullFlow then
            if not (newStates.ContainsKey(("X","X",opened)))
            then newStates.Add(("X","X",opened), currentFlow)
            elif newStates[("X","X",opened)] < currentFlow
            then newStates[("X","X",opened)] <- currentFlow
            
        else
            let myValve = map[myPos]
            let kentsValve = map[kentsPos]
            let myMoves = 
                myValve.Tunnels
                |>> (fun nextPos -> ((nextPos,opened),0))
                |> Array.append 
                    (
                        if myValve.FlowRate > 0 && not(Set.contains myPos opened ) 
                        then [|((myPos,Set.add myPos opened), myValve.FlowRate * timeLeft )|]
                        else [||]
                    )
            let kentsMoves = 
                kentsValve.Tunnels
                |>> (fun nextPos -> ((nextPos,opened),0))
                |> Array.append
                    (
                        if kentsValve.FlowRate > 0 && not(Set.contains kentsPos opened ) 
                        then [|((kentsPos,Set.add kentsPos opened), kentsValve.FlowRate * timeLeft )|]
                        else [||]
                    )
            myMoves
            |> Array.iter (fun ((myPos,myOpened),myFlow) ->
                kentsMoves
                |> Array.iter (fun ((kentsPos,kentsOpened),kentsFlow) ->
                    let flow = currentFlow + if myPos = kentsPos then myFlow else myFlow + kentsFlow
                    let opened = Set.union myOpened kentsOpened
                    let (p1, p2) = 
                        if opened = fullFlow then ("X", "X") 
                        elif myPos < kentsPos then (myPos,kentsPos) 
                        else (kentsPos, myPos)
                    if not (newStates.ContainsKey((p1,p2,opened)))
                    then newStates.Add((p1,p2,opened), flow)
                    elif newStates[(p1,p2,opened)] < flow
                    then newStates[(p1,p2,opened)] <- flow
                )
            )
    newStates

let part1 input =
    let map = parseInput input
    let mutable paths = Map.empty |> Map.add ("AA", Set.empty) 0
    {1..30}
    |> Seq.iter (fun i ->
        paths <- step map (30 - i) paths
        Console.WriteLine $"After {i} minute(s). {Map.count paths} paths ..."
    )
    paths |> Map.values |> Seq.max
    |> sprintf "Flow: %i"


let part2 input = 
    let map = parseInput input
    let fullFlow = 
        map |> Map.toSeq 
        |> Seq.where (fun (_, valve) -> valve.FlowRate > 0) |>> fst
        |> Set.ofSeq

    let mutable paths = new Dictionary<(string*string*(Set<string>)), int>()
    let mutable newPaths = new Dictionary<(string*string*(Set<string>)), int>()
    paths.Add(("AA", "AA", Set.empty), 0)
    {1..26}
    |> Seq.iter (fun i ->
        paths <- stepWithKent map fullFlow (26 - i) &paths
        //let keys = paths.Keys
        //newPaths.Clear()
        //let timeLeft = (26 - i)

        //keys
        //|> Seq.iter (fun (myPos,kentsPos,opened) ->
        //    let currentFlow = paths[(myPos,kentsPos,opened)]
        //    if opened = fullFlow then
        //        if not (newPaths.ContainsKey(("X","X",opened)))
        //        then newPaths.Add(("X","X",opened), currentFlow)
        //        elif newPaths[("X","X",opened)] < currentFlow
        //        then newPaths[("X","X",opened)] <- currentFlow
            
        //    else
        //        let myValve = map[myPos]
        //        let kentsValve = map[kentsPos]
        //        let myMoves = 
        //            myValve.Tunnels
        //            |>> (fun nextPos -> ((nextPos,opened),0))
        //            |> Array.append 
        //                (
        //                    if myValve.FlowRate > 0 && not(Set.contains myPos opened ) 
        //                    then [|((myPos,Set.add myPos opened), myValve.FlowRate * timeLeft )|]
        //                    else [||]
        //                )
        //        let kentsMoves = 
        //            kentsValve.Tunnels
        //            |>> (fun nextPos -> ((nextPos,opened),0))
        //            |> Array.append
        //                (
        //                    if kentsValve.FlowRate > 0 && not(Set.contains kentsPos opened ) 
        //                    then [|((kentsPos,Set.add kentsPos opened), kentsValve.FlowRate * timeLeft )|]
        //                    else [||]
        //                )
        //        myMoves
        //        |> Array.iter (fun ((myPos,myOpened),myFlow) ->
        //            kentsMoves
        //            |> Array.iter (fun ((kentsPos,kentsOpened),kentsFlow) ->
        //                let flow = currentFlow + if myPos = kentsPos then myFlow else myFlow + kentsFlow
        //                let opened = Set.union myOpened kentsOpened
        //                let (p1, p2) = 
        //                    if opened = fullFlow then ("X", "X") 
        //                    elif myPos < kentsPos then (myPos,kentsPos) 
        //                    else (kentsPos, myPos)
        //                if not (newPaths.ContainsKey((p1,p2,opened)))
        //                then newPaths.Add((p1,p2,opened), flow)
        //                elif newPaths[(p1,p2,opened)] < flow
        //                then newPaths[(p1,p2,opened)] <- flow
        //            )
        //        )
        //)
        Console.WriteLine $"After {i} minute(s). {paths.Count} paths ..."
    )
    paths.Values |> Seq.max
    |> sprintf "Flow: %i"

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
