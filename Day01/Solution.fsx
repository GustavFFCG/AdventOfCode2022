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

let parseInput =
    Seq.fold
        (fun state s ->
            match s with
            | "" -> [] :: state
            | input -> (int input :: state.Head) :: state.Tail
        )
        [[]]
    >> List.rev

let elfLoads =
    fileName
    |> Result.bind readFile
    |> Result.map parseInput
 
let maxCalories =
    List.map List.sum
    >> List.max

let part1 = 
    elfLoads 
    |> Result.map maxCalories
    |> function
    | Ok i -> $"Max calories by one elf is %i{i}"
    | Error s -> $"Error: %s{s}"

Console.WriteLine part1

let sumOfTopLoads n : (int list list -> int) =
    List.map List.sum
    >> List.sortDescending
    >> List.take n
    >> List.sum

let part2 =
    let nOfTop = 3
    elfLoads
    |> Result.map (sumOfTopLoads nOfTop)
    |> function
    | Ok i -> $"Top %i{nOfTop} Elves carried %i{i} calories."
    | Error s -> $"Error: %s{s}"

Console.WriteLine part2
