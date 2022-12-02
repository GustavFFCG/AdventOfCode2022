open System
open System.IO
open Microsoft.FSharp.Collections

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

type Shape =
| Rock
| Paper
| Scissors

type Round = {
    Opponent : Shape
    You : Shape
}

type Outcome =
| Win
| Lose
| Draw

module Shape =
    let score = function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
module Outcome =
    let score = function
        | Win -> 6
        | Lose -> 0
        | Draw -> 3

module Round =
    let ofString (s:string) =
        let opponent = function
            | 'A' -> Rock
            | 'B' -> Paper
            | 'C' -> Scissors
            | other -> failwith $"Unexpected input %c{other}"

        let you = function
            | 'X' -> Rock
            | 'Y' -> Paper
            | 'Z' -> Scissors
            | other -> failwith $"Unexpected input %c{other}"

        {Opponent = opponent s.[0] ; You = you s.[2]}

    let ofString2 (s:string) =
        let opponent = function
            | 'A' -> Rock
            | 'B' -> Paper
            | 'C' -> Scissors
            | other -> failwith $"Unexpected input %c{other}"

        let you opponent c =
            match opponent, c with
            | Rock, 'X' -> Scissors
            | Rock, 'Y' -> Rock
            | Rock, 'Z' -> Paper
            | Paper, 'X' -> Rock
            | Paper, 'Y' -> Paper
            | Paper, 'Z' -> Scissors
            | Scissors, 'X' -> Paper
            | Scissors, 'Y' -> Scissors
            | Scissors, 'Z' -> Rock
            | _, other -> failwith $"Unexpected input %c{other}"

        {Opponent = opponent s.[0] ; You = you (opponent s.[0]) s.[2]}

    let outcome = function
        | {Opponent = Rock ; You = Rock} -> Draw
        | {Opponent = Rock ; You = Paper} -> Win
        | {Opponent = Rock ; You = Scissors} -> Lose
        | {Opponent = Paper ; You = Rock} -> Lose
        | {Opponent = Paper ; You = Paper} -> Draw
        | {Opponent = Paper ; You = Scissors} -> Win
        | {Opponent = Scissors ; You = Rock} -> Win
        | {Opponent = Scissors ; You = Paper} -> Lose
        | {Opponent = Scissors ; You = Scissors} -> Draw

    let score round =
        (round |> outcome |> Outcome.score)
        + (Shape.score round.You)

let parseInput(input: string seq) =
    input
    |> Seq.map Round.ofString

let parseInput2(input: string seq) =
    input
    |> Seq.map Round.ofString2

let part1 = 
    fileName
    |> Result.bind readFile
    |> Result.map parseInput
    |> Result.map (Seq.map Round.score)
    |> Result.map Seq.sum
    |> function
    | Ok i -> $"Total score part 1 is %i{i}"
    | Error s -> $"Error: %s{s}"

Console.WriteLine part1

let part2 = 
    fileName
    |> Result.bind readFile
    |> Result.map parseInput2
    |> Result.map (Seq.map Round.score)
    |> Result.map Seq.sum
    |> function
    | Ok i -> $"Total score part 2 is %i{i}"
    | Error s -> $"Error: %s{s}"

Console.WriteLine part2
