#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus

type Distance =
| Preliminary of int
| Final of int

type SquareType = 
| Start
| End
| Intermediate of char
with 
    static member ofChar = function
        | 'S' -> Start
        | 'E' -> End
        | c -> Intermediate c

type Square = {
    Type: SquareType
    Distance: Distance
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
    input |> array2D 
    |> Array2D.map (fun c ->
        let squareType = SquareType.ofChar c
        {
            Type = squareType
            Distance = 
                match squareType with 
                | Start -> Preliminary 0
                | _ -> Preliminary (System.Int32.MaxValue)
        }
    )

let djikstra (pos: int*int) (map:Square[,]) =
    

let answer1 = 
    monad {
        let! map = fileName >>= readFile |>> parseInput 
        return 0
    }

let part1 = answer1 |>> sprintf "Shortest route: %i"

let part2 = Ok "todo"

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
