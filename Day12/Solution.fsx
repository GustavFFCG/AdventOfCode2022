#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus

type Distance =
| Unknown
| Preliminary of int
| Final of int

module Distance =
    let isFinal = function | Final _ -> true |_ -> false
    let finalRisk = function | Final r -> Some r | _ -> None
    
type Coord = {x: int; y:int}
    with static member ofTuple (x,y) = {x=x; y=y}

type SquareType = 
| Start
| End
| Intermediate of char
with 
    static member ofChar = function
        | 'S' -> Start
        | 'E' -> End
        | c -> Intermediate c

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

let parseInput (input: string seq) : SquareType[,] =
    input |>> (Seq.map SquareType.ofChar) |> array2D

module Map =
    let arr = fileName >>= readFile |>> parseInput |> Result.defaultWith (fun s -> failwith s)

    let width = arr |> Array2D.length1
    let height = arr |> Array2D.length2

    let maxHeight = 
        arr |> Seq.cast<SquareType>
        |> Seq.choose (function | Intermediate c -> Some (int c) | _ -> None)
        |> Seq.max

    let findSquare kind  =
        let rec go x y =
              if   y >= height then None
              elif x >= width then go 0 (y+1)
              elif arr[x,y] = kind then Some (x,y)
              else go (x+1) y
        go 0 0
        |>> Coord.ofTuple
        |> Option.defaultWith (fun () -> failwith $"Square of kind {kind} not found")

    let findSquares kind  =
        let rec go acc x y =
              if   y >= height then acc
              elif x >= width then go acc 0 (y+1)
              elif arr[x,y] = kind then go ((x,y)::acc) (x+1) y
              else go acc (x+1) y
        go [] 0 0
        |>> Coord.ofTuple

    let adjacentCoordinates coord =
        [
            coord.x-1, coord.y
            coord.x, coord.y-1; coord.x,coord.y+1
            coord.x+1, coord.y
        ]
        |> List.map Coord.ofTuple
        |> List.filter (fun c -> 
            c.x >= 0 && c.y >= 0 && c.x < width && c.y < height)
        |> List.filter (fun c ->
            (arr[coord.x, coord.y], arr[c.x, c.y])|> function
                | (Intermediate curr, Intermediate dest) -> (int dest) - (int curr) <= 1
                | (Intermediate curr, End) when (int curr) < maxHeight -> false
                | (_, _) -> true)

    let aggregateDistance coord m = m |> Map.tryFind coord |> Option.defaultValue Unknown
    
    let isComplete (m: Map<Coord,Distance>) = 
        not(m |> Map.exists (fun c distance -> not (Distance.isFinal distance) ))

module Dict =
    let isComplete (m: System.Collections.Generic.Dictionary<Coord,Distance>) =
        not(m |> Seq.exists (fun x -> not (Distance.isFinal x.Value)))
    let aggregateDistance coord (m: System.Collections.Generic.Dictionary<Coord,Distance>) = 
        if m.ContainsKey coord then m[coord] else Unknown

let rec dijkstra  (map: Map<Coord,Distance>) =
    if Map.isComplete map then map
    else
        let minFinalNode =
            map
            |> Map.toSeq
            |> Seq.map (fun (c, dist) ->
                match dist with
                | Preliminary r -> Some (c,r) 
                | _ -> None)
            |> Seq.choose id
            |> Seq.minBy (fun (_c, i) -> i)
        
        let newMap = 
            Map.adjacentCoordinates (fst minFinalNode)
            |> List.filter (fun c -> 
                map |> Map.aggregateDistance c |> Distance.isFinal |> not
            )
            |> List.fold (fun state c ->
                let aggregate = Map.aggregateDistance c map
                let dist = 1
                let newAggregate = dist + (snd minFinalNode)
                match aggregate with
                    | Unknown -> 
                        state |> Map.add c (Preliminary newAggregate)
                    | Preliminary r when r > newAggregate ->
                        state |> Map.add c (Preliminary newAggregate)
                    | _ -> state
                ) map
            |> Map.add (fst minFinalNode) (Final (snd minFinalNode))
        dijkstra newMap

let rec mutableDijkstra  (map: byref<System.Collections.Generic.Dictionary<Coord,Distance>>) =
    if not (Dict.isComplete map) then map
    else
        let minFinalNode =
            map
            |> Seq.map (fun x ->
                match x.Value with
                | Preliminary r -> Some (x.Key,r) 
                | _ -> None)
            |> Seq.choose id
            |> Seq.minBy (fun (_c, i) -> i)
        
        let adjacent = Map.adjacentCoordinates (fst minFinalNode)
        for coord in adjacent do
            let aggregate = Dict.aggregateDistance coord map
            if not (Distance.isFinal aggregate) then
                let cost = 1
                let newAggregate = cost + (snd minFinalNode)
                match aggregate with
                    | Unknown -> 
                        map[coord] <- (Preliminary newAggregate)
                    | Preliminary r when r > newAggregate ->
                        map[coord] <- (Preliminary newAggregate)
                    | _ -> ()
        mutableDijkstra &map
    

let answer1 = 
    let startingMap = Map.empty |> Map.add (Map.findSquare Start) (Preliminary 0)
    dijkstra startingMap
    |> Map.find (Map.findSquare End)
    |> function
    | Final i -> Ok i
    | _ -> Error "No path found"

let answer2 =
    let endSquare = Map.findSquare End

    (Map.findSquare Start)::(Map.findSquares (Intermediate 'a'))
    |>> (fun startingSquare ->
        let startingMap = Map.empty |> Map.add startingSquare (Preliminary 0)
        dijkstra startingMap
    )
    |>> (fun map ->
        if (Map.containsKey endSquare map) then
            Map.find endSquare map
            |> function
            | Final i -> Some i
            | _ -> None
        else None
    )
    |> List.choose id
    |> function | [] -> Error "no solutions" | l -> List.min l |> Ok

let answer2b =
    let endSquare = Map.findSquare End

    (Map.findSquare Start)::(Map.findSquares (Intermediate 'a'))
    |>> (fun startingSquare ->
        let startingMap = new System.Collections.Generic.Dictionary<Coord,Distance>(startingSquare, (Preliminary 0))
        mutableDijkstra startingMap
    )
    |>> (fun map ->
        if (Map.containsKey endSquare map) then
            Map.find endSquare map
            |> function
            | Final i -> Some i
            | _ -> None
        else None
    )
    |> List.choose id
    |> function | [] -> Error "no solutions" | l -> List.min l |> Ok

let part1 = answer1 |>> sprintf "Shortest route from start: %i"

let part2 = answer2 |>> sprintf "Shortest route from start or a: %i"

module Tests =
    let private tests = 
        [
            //fun () -> Error "todo"
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
