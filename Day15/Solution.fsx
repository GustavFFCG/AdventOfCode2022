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
let parseInput input =
    input
    |> Seq.map (fun s ->
        let intpat = "\-?[0-9]*"
        let m = Regex.Match(s, $"Sensor at x=({intpat}), y=({intpat}): closest beacon is at x=({intpat}), y=({intpat})")
        if m.Success then {Pos = (int m.Groups[1].Value, int m.Groups[2].Value); BeaconPos = (int m.Groups[3].Value,int m.Groups[4].Value) }
        else failwith "Unexpected input"
    )

let manhattanDistance ((x1,y1):(int*int)) ((x2,y2):(int*int)) = Math.Abs (x1 - x2) + Math.Abs (y1 - y2)

let posIsScannedBySensor x y (sensor: Sensor) =
    manhattanDistance (x,y) sensor.Pos <= manhattanDistance sensor.BeaconPos sensor.Pos

let posIsScannedByAnySensor x y (sensors: Sensor seq) =
    sensors |> Seq.exists (fun s -> posIsScannedBySensor x y s)

let scannedAtRow y sensors =
    let minx =
        sensors
        |>> (fun x -> 
            Math.Min((fst x.BeaconPos), (2* (fst x.Pos) - (fst x.BeaconPos))))
        |> Seq.min
    let maxx =
        sensors
        |>> (fun x -> 
            Math.Max((fst x.BeaconPos), (2* (fst x.Pos) - (fst x.BeaconPos))))
        |> Seq.max

    {minx..maxx} 
    |> Seq.where (fun x -> posIsScannedByAnySensor x y sensors)
    |> Seq.length
let part1 input =
    let sensors = parseInput input
    scannedAtRow 10 sensors
    |> sprintf "%i positions"

let part2 input = 
    "todo"

module Tests =
    let private tests = 
        [
            fun () -> manhattanDistance (0,0) (5,5) |> function 10 -> Ok () | x -> Error $"Dist was {x}"
            fun () -> {Pos = (5,10); BeaconPos = (6,10)} |> posIsScannedBySensor 6 10 |> function true -> Ok () | x -> Error $"Error"
            fun () ->
                [|
                    "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
                    "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
                    "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
                |] 
                |> parseInput
                |> scannedAtRow 10 |> function 21 -> Ok () | x -> Error $"Was {x}"
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
