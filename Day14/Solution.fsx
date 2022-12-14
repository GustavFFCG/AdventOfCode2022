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
                if lastx = x then {lasty..y} |> Seq.map (fun y -> (x,y)) //math abs!
                elif lasty = y then {lastx..x} |> Seq.map (fun x -> (x,y))
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

let rec restingPos (map: (int*int) Set) (x,y) =
    if willFallOff map (x,y) then None
    elif not Set.contains (x, (y + 1)) map 
        && Set.contains ((x), (y + 1)) map
let part1 input =
    let map = parseInput input


let part2 input = 
    Ok "todo"

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
        (part1 input)
        (part2 input)
| None ->
    Tests.run () |>> sprintf "Tests only:\r\n %s"
|> function
| Ok s -> s
| Error s -> sprintf "Error: %s" s 
|> Console.WriteLine 
