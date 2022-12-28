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

let parseInput strings =
    strings
    |> Seq.map (fun row -> 
        row |> String.split [| ","|] 
        |> Seq.map int |> List.ofSeq
        |> function | [x;y;z] -> (x, y, z) | _ -> failwith "unexpected input"
    )

let adjacentPoints (x, y, z) =
    [
        (x-1, y, z)
        (x+1, y, z)
        (x, y-1, z)
        (x, y+1, z)
        (x, y, z-1)
        (x, y, z+1)
    ] |> Set.ofList

let surfaceArea scan =
    scan
    |> Seq.fold
        (fun sum point ->
            sum + (Set.difference (adjacentPoints point) scan |> Set.count)
        )
        0

let exteriorSurfaceArea steam scan =
    scan
    |> Seq.fold
        (fun sum point ->
            sum + (Set.intersect (adjacentPoints point) steam |> Set.count)
        )
        0
    
let part1 input =
    parseInput input |> Set.ofSeq
    |> surfaceArea
    |> sprintf "Surface area %i"

let enclosingSpace (scan: Set<int*int*int>) =
    let minx, maxx  = scan |>> (fun (x, y, z) -> x) |> fun x -> (Seq.min x, Seq.max x)
    let miny, maxy  = scan |>> (fun (x, y, z) -> y) |> fun y -> (Seq.min y, Seq.max y)
    let minz, maxz  = scan |>> (fun (x, y, z) -> z) |> fun z -> (Seq.min z, Seq.max z)
    let startingPoint = 
        if Set.contains (minx, miny, minz) scan 
        then failwith "Use another starting point" else (minx, miny, minz)
    let volume = (maxx - minx + 2) * (maxy - miny + 2) * (maxz - minz + 2)

    let mutable steam = Set.empty

    let rec fill point =
        if 
            Set.contains point steam
            || Set.contains point scan
            || not (point |> fun (x, y, z) -> 
                        x >= minx - 1 && x <= maxx + 1
                        && y >= miny - 1 && y <= maxy + 1
                        && z >= minz - 1 && z <= maxz + 1)
        then ()
        else
            steam <- Set.add point steam
            let adjacent = adjacentPoints point
            for point' in adjacent do
                fill point'

    fill startingPoint
    steam

let part2 input =
    let scan = parseInput input |> Set.ofSeq
    let steam = scan |> enclosingSpace
    exteriorSurfaceArea steam scan
    |> sprintf "Exterior surface area %i"

module Tests =
    let private tests = 
        [
            fun () -> 
                [| (1,1,1); (2,1,1)|] |> Set.ofSeq |> surfaceArea
                |> function | 10 -> Ok () | other -> Error $"Expected 10 was {other}"
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
