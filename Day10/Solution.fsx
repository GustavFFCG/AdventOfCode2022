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
    if m.Success then m.Groups[1].Value |> int |> Some else None

let processInstructions instructions =
    instructions
    |> Seq.fold
        (fun (x, acc) instruction -> 
            match instruction with
            | "noop" -> (x, x::acc)
            | Addx i -> (x + i , x::x::acc)
            | other -> failwith "unexpected input: {other}"
        )
        (1, [])
    |> snd
    |> rev

let signalStrength (signals: int list) i =
    signals[i - 1] * i

let part1 = 
    fileName
    >>= readFile
    |>> processInstructions
    |>> (fun inst -> [20;60;100;140;180;220] |> List.sumBy (signalStrength inst) )
    |>> sprintf "Sum is %i"

let drawPixel (i: int) spritePos =
    if Math.Abs ((i%40) - spritePos) > 1 then '.' else '#'

let part2 =
    fileName
    >>= readFile
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
