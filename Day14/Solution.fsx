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

let setOfCoords (coords: (int*int) seq) =
    coords
    |> Seq.tail
    |> Seq.fold 
        (fun (set,(lastx, lasty)) (x,y) ->
            let line =
                if lastx = x then
                    (if lasty <= y then {lasty..y} else {y..lasty}) 
                    |> Seq.map (fun y -> (x,y))
                elif lasty = y then 
                    (if lastx <= x then {lastx..x} else {x..lastx}) 
                    |> Seq.map (fun x -> (x,y))
                else failwith "not a straight line"
                |> Set.ofSeq
            (set |> Set.union line, (x,y)) 
        )
        (Set.empty, (Seq.head coords))
        |> fst

let parseInput (input: string seq) =
    input
    |>> (fun s ->
        s |> String.split [|" -> "|]
        |>> (String.split [|","|] >> List.ofSeq >> (fun l -> int l[0], int l[1] ))
    )
    |> Seq.map setOfCoords
    |> Set.unionMany

let willFallOff (map: (int*int) Set) (x,y) =
    map |> Set.exists (fun (x',y') -> x' = x && y' > y) |> not

let rec restingPos1 (map: (int*int) Set) (x,y) =
    if willFallOff map (x,y) then None
    elif not (Set.contains (x, (y + 1)) map)   then restingPos1 map (x, (y + 1))
    elif not (Set.contains (x-1, (y + 1)) map) then restingPos1 map (x-1, (y + 1))
    elif not (Set.contains (x+1, (y + 1)) map) then restingPos1 map (x+1, (y + 1))
    else Some (x,y)

let floor (map: (int*int) Set) =
    (map |> Set.toSeq |> Seq.map snd |> Seq.max) + 2

let rec restingPos2 floor (map: (int*int) Set) (x,y) =
    if Set.contains (x,y) map then None
    elif y = floor - 1 then Some (x,y)
    elif not (Set.contains (x, (y + 1)) map)   then restingPos2 floor map (x, (y + 1))
    elif not (Set.contains (x-1, (y + 1)) map) then restingPos2 floor map (x-1, (y + 1))
    elif not (Set.contains (x+1, (y + 1)) map) then restingPos2 floor map (x+1, (y + 1))
    else Some (x,y)

let rec pourGrain restFun (x,y) (map: (int*int) Set) =
    match restFun map (x,y) with
    | Some (x',y') -> map |> Set.add (x',y') |> pourGrain restFun (x,y)
    | None -> map

let part1 input =
    let map = parseInput input
    let filledMap = map |> pourGrain restingPos1 (500,0)
    Console.WriteLine $"{Set.count filledMap} - {Set.count map} "
    (Set.count filledMap) - (Set.count map)
    |> sprintf "%i grains"

let part2 input = 
    let map = parseInput input
    let floor = floor map
    let filledMap = map |> pourGrain (restingPos2 floor) (500,0)
    Console.WriteLine $"{Set.count filledMap} - {Set.count map} "
    (Set.count filledMap) - (Set.count map)
    |> sprintf "%i grains"

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
