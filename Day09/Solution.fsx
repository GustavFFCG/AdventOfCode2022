#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus

type Direction = 
    | Up
    | Down
    | Right
    | Left

type ShortRopestate = {
    HeadPos : int * int
    TailPos : int * int
}

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

let parseInput (input: string seq) =
    input
    |>> (fun (s: string) -> 
        match s[0] with 
            | 'U' -> Up
            | 'D' -> Down
            | 'R' -> Right
            | 'L' -> Left
            | _ -> failwith "Unexpected direction"
        , int (s.Substring 2))
    >>= (fun (dir, steps) -> seq{1..steps} |>> fun _ -> dir )

let move pos ((x:int),(y:int)) =
    match pos with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Right -> (x + 1, y)
    | Left -> (x - 1, y)

let moveHead direction state = {state with HeadPos = state.HeadPos |> move direction}
let moveTail direction state = {state with TailPos = state.TailPos |> move direction}

let resolveTension ropestate =
    ropestate
    |> (fun x -> 
        if fst x.TailPos < (fst x.HeadPos - 1) || (fst x.TailPos < fst x.HeadPos && Math.Abs (snd ropestate.TailPos - snd ropestate.HeadPos) > 1)
        then x |> moveTail Right else x)
    |> (fun x -> 
        if fst x.TailPos > (fst x.HeadPos + 1) || (fst x.TailPos > fst x.HeadPos && Math.Abs (snd ropestate.TailPos - snd ropestate.HeadPos) > 1)
        then x |> moveTail Left else x)
    |> (fun x -> 
        if snd x.TailPos < (snd x.HeadPos - 1) || (snd x.TailPos < snd x.HeadPos && Math.Abs (fst ropestate.TailPos - fst ropestate.HeadPos) > 1)
        then x |> moveTail Up else x)
    |> (fun x -> 
        if snd x.TailPos > (snd x.HeadPos + 1) || (snd x.TailPos > snd x.HeadPos && Math.Abs (fst ropestate.TailPos - fst ropestate.HeadPos) > 1)
        then x |> moveTail Down else x)

let step (state:ShortRopestate) direction =
    state 
    |> moveHead direction
    |> resolveTension

let stepLong (state:(int*int) list ) direction =
    let head, tail = move direction (head state), List.tail state
    tail
    |> List.fold
        (fun state pos -> 
            let newPos = 
                { HeadPos = state |> rev |> List.head; TailPos = pos } 
                |> resolveTension
                |> fun x -> x.TailPos
            state @ [newPos]
        )
        [head]
    
let calcPart1 (input: Direction seq) = 
    input
    |> Seq.fold 
        (fun (ropeState, acc) dir ->
            let newstate = step ropeState dir
            (newstate, acc |> Set.add newstate.TailPos))
        ({HeadPos = (0,0); TailPos = (0,0)}, Set.ofList [(0,0)])
    |> snd
    |> Set.count

let calcPart2 (input: Direction seq) = 
    let startingRope = seq{1..10} |>> (fun _ -> (0,0)) |> List.ofSeq
    input
    |> Seq.fold 
        (fun (ropeState, acc) dir ->
            let newstate = stepLong ropeState dir
            (newstate, acc |> Set.add (head (rev newstate))))
        (startingRope, Set.ofList [(0,0)])
    |> snd
    |> Set.count

let part1 =
    fileName
    >>= readFile
    |>> parseInput
    |>> calcPart1
    |>> sprintf "Part one: %i positions"

let part2 =
    fileName
    >>= readFile
    |>> parseInput
    |>> calcPart2
    |>> sprintf "Part two: %i positions"

module Tests =
    let private tests = 
        [
            fun () -> 
                let start = {HeadPos = (1,1); TailPos = (0,0)}
                [
                    Up, {HeadPos = (1,2); TailPos = (1,1)}
                    Down, {HeadPos = (1,0); TailPos = (0,0)}
                    Right, {HeadPos = (2,1); TailPos = (1,1)}
                    Left, {HeadPos = (0,1); TailPos = (0,0)}
                ]
                |> List.fold 
                    (fun state (dir, expected) -> 
                        let actual = step start dir
                        state >>= fun () -> (if actual = expected then Ok() else Error $"Moved {dir} from {start}. Expected {expected}, was {actual}") )
                    (Ok ())
            fun () ->
                let start = {HeadPos = (1,1); TailPos = (0,1)}
                [
                    Up, {HeadPos = (1,2); TailPos = (0,1)}
                    Down, {HeadPos = (1,0); TailPos = (0,1)}
                    Right, {HeadPos = (2,1); TailPos = (1,1)}
                    Left, {HeadPos = (0,1); TailPos = (0,1)}
                ]
                |> List.fold 
                    (fun state (dir, expected) -> 
                        let actual = step start dir
                        state >>= fun () -> (if actual = expected then Ok() else Error $"Moved {dir} from {start}. Expected {expected}, was {actual}") )
                    (Ok ())
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
