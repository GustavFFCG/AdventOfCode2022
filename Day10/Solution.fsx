#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus
open System.Text.RegularExpressions

type Instruction = 
    | Noop
    | Addx of int

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

let (|Addx|_|) str =
    let m = Regex.Match(str, "addx (\-?[0-9]*)")
    if m.Success then m.Groups[1].Value |> int |> Instruction.Addx |> Some else None

let parseInput (input: string seq) =
    input |>> function | "noop" -> Noop | Addx a -> a | other -> failwith $"unexpected input {other}"

let processInstructions (instructions: Instruction seq) =
    instructions
    |> Seq.fold
        (fun (x, acc) instruction -> 
            match instruction with
            | Noop -> (x, x::acc)
            | Instruction.Addx i -> (x + i , x::x::acc)
        )
        (1, [])
    |> snd
    |> rev

let signalStrength i (signals: int list) =
    signals[i - 1] * i

let part1 = 
    fileName
    >>= readFile
    |>> parseInput
    |>> processInstructions
    |>> (fun inst -> 
        (signalStrength 20 inst) 
        + (signalStrength 60 inst) 
        + (signalStrength 100 inst) 
        + (signalStrength 140 inst) 
        + (signalStrength 180 inst) 
        + (signalStrength 220 inst)
        )
    |>> sprintf "Sum is %i"

let drawPixel (i: int) spritePos =
    if Math.Abs ((i%40) - spritePos) > 1 then '.' else '#'

let part2 =
    fileName
    >>= readFile
    |>> parseInput
    |>> processInstructions
    |>> List.mapi drawPixel
    |>> (List.splitInto 6)
    |>> (List.map ( Array.ofList >> String))
    |>> (String.concat Environment.NewLine)
    |>> sprintf "%s%s" Environment.NewLine


module Tests =
    let private tests = 
        [
            fun () -> drawPixel 0 1 |> function | '#' -> Ok () | c -> Error $"{c}"
            fun () -> drawPixel 40 1 |> function | '#' -> Ok () | c -> Error $"{c}"
            fun () -> drawPixel 40 3 |> function | '.' -> Ok () | c -> Error $"{c}"
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
