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
type Sensor = {
    Pos: int * int
    BeaconPos: int*int
}
type LineSegment = {Min:int; Max: int}
    with 
    static member join (ls1: LineSegment) (ls2: LineSegment) =
        {Min = Math.Min (ls1.Min, ls2.Min); Max = Math.Max (ls1.Max, ls2.Max)}
    static member add (ls: LineSegment) (acc: LineSegment Set) =
        let intersectingLines =
            acc 
            |> Seq.where (fun (ls':LineSegment) -> 
                ((ls'.Min <= ls.Max) && (ls'.Max >= ls.Min)) || ((ls'.Max >= ls.Min) && (ls'.Min <= ls.Max))
                )
            |> Set.ofSeq
        let combinedLine =
            intersectingLines |> Seq.fold LineSegment.join ls

        Set.difference acc intersectingLines
        |> Set.add combinedLine

    static member length (ls: LineSegment) = ls.Max - ls.Min

let parseInput input =
    input
    |> Seq.map (fun s ->
        let intpat = "\-?[0-9]*"
        let m = Regex.Match(s, $"Sensor at x=({intpat}), y=({intpat}): closest beacon is at x=({intpat}), y=({intpat})")
        if m.Success then {Pos = (int m.Groups[1].Value, int m.Groups[2].Value); BeaconPos = (int m.Groups[3].Value,int m.Groups[4].Value) }
        else failwith "Unexpected input"
    )

let manhattanDistance ((x1,y1):(int*int)) ((x2,y2):(int*int)) = Math.Abs (x1 - x2) + Math.Abs (y1 - y2)

let toLineSegmentAt y (sensor: Sensor) =
    let vertDist = Math.Abs (y - (snd sensor.Pos))
    let horizDist = manhattanDistance sensor.BeaconPos sensor.Pos - vertDist
    if horizDist >= 0 then Some {Min = (fst sensor.Pos) - horizDist ; Max = (fst sensor.Pos) + horizDist }
    else None

let part1 input =
    let sensors = parseInput input
    sensors
    |>> (toLineSegmentAt 2000000)
    |> Seq.choose id
    |> Seq.fold (fun state ls -> LineSegment.add ls state)  Set.empty
    |> Set.toSeq
    |> Seq.sumBy LineSegment.length
    |> sprintf "scanned: %i"

let part2 input = 
    let sensors = parseInput input
    let mutable y = 0
    let beaconPos y =
        sensors
        |>> (toLineSegmentAt y)
        |> Seq.choose id
        |> Seq.fold (fun state ls -> LineSegment.add ls state)  Set.empty
        |> Set.toList
        |> function 
        | [{Min=min;Max=max}] when min <= 0 && max >= 4000000 -> None
        | [a;b] -> Math.Min(a.Max, b.Max) + 1 |> Some 
        | other -> failwith $"error {other}"
    let mutable x = beaconPos 0
    while x = None && y < 4000000 do
        if y % 100000 = 0 then Console.WriteLine $"At {y}..."
        y <- y + 1
        x <- beaconPos y
    x |> Option.defaultWith (fun () -> failwith "not found")
    |> fun x -> (int64 x) * 4000000L + (int64 y)
    |> sprintf "Tuning freq: %i"

    

module Tests =
    let private tests = 
        [
            fun () -> manhattanDistance (0,0) (5,5) |> function 10 -> Ok () | x -> Error $"Dist was {x}"
            fun () ->
                toLineSegmentAt 10 {Pos = (5,10); BeaconPos = (6,10)}
                |> function Some {Min = 4; Max = 6} -> Ok () | other -> Error $"Faulty LS {other}"
            fun () -> 
                toLineSegmentAt 0 {Pos = (8,7); BeaconPos = (2,10)}
                |> function Some {Min = 6; Max = 10} -> Ok () | other -> Error $"Faulty LS {other}"
            fun () -> 
                toLineSegmentAt 10 {Pos = (8,17); BeaconPos = (9,17)}
                |> function None -> Ok () | other -> Error $"Faulty LS {other}"
            fun () -> 
                LineSegment.join { Min = -2; Max = 18 } { Min = 16; Max = 24 } 
                |> function { Min = -2; Max = 24 } -> Ok () | other -> Error $"Faulty join {other}"
            fun () -> 
                [|{ Min = 16; Max = 24 } |] |> Set.ofSeq |> LineSegment.add { Min = -2; Max = 18 } |> Set.toList
                |> function [{ Min = -2; Max = 24 }] -> Ok () | other -> Error $"Faulty join {other}"
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
