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
    | _::s::_ -> Ok s 
    | _ -> Error "Please provide a filename for input"

let readFile fileName =
    try
        File.ReadLines fileName
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}"

let parseInput (input: string seq) =
    input |> Seq.map (Seq.map int) |> array2D

let findHidden (matrix: int[,]) =
    matrix
    |> Array2D.mapi (fun x y i ->
        let length1 = Array2D.length1 matrix
        let length2 = Array2D.length2 matrix
        if x = 0 || y = 0 || x = (length1 - 1) || y = (length2 - 1) then false
        else
            let ``hidden from top`` = (Seq.max matrix[x,0..(y - 1)] >= i)
            let ``hidden from bottom`` = (Seq.max matrix[x,(y + 1)..(length2 - 1)] >= i)
            let ``hidden from left`` = (Seq.max matrix[0..(x - 1),y] >= i)
            let ``hidden from right`` = (Seq.max matrix[(x + 1)..(length1 - 1),y] >= i)
            ``hidden from top`` && ``hidden from bottom`` && ``hidden from left`` && ``hidden from right``
    )

let scenicScore (matrix: int[,]) =
    matrix
    |> Array2D.mapi (fun x y i ->
        let length1 = Array2D.length1 matrix
        let length2 = Array2D.length2 matrix
        if x = 0 || y = 0 || x = (length1 - 1) || y = (length2 - 1) then 0
        else
            let calcScene defaultValue arr = 
                arr |> Seq.tryFindIndex (fun height -> height >= i) |>> (fun score -> score + 1) |> Option.defaultValue defaultValue
            let ``scene to top`` = 
                matrix[x,0..(y - 1)] 
                |> Seq.rev
                |> calcScene y
            let ``scene to bottom`` = 
                matrix[x,(y + 1)..(length2 - 1)]
                |> calcScene (length2 - y - 1)
            let ``scene to left`` = 
                matrix[0..(x - 1),y]
                |> Seq.rev 
                |> calcScene x
            let ``scene to right`` = 
                matrix[(x + 1)..(length1 - 1),y]
                |> calcScene (length1 - x - 1)

            ``scene to top`` * ``scene to bottom`` * ``scene to left`` * ``scene to right``
    )

let part1 =
    fileName
    >>= readFile
    |>> parseInput
    |>> findHidden
    |>> (Seq.cast >> Seq.where not >> Seq.length)
    |>> sprintf "Number of hidden is %i"

let part2 =
    fileName
    >>= readFile
    |>> parseInput
    |>> scenicScore
    |>> (Seq.cast >> Seq.max)
    |>> sprintf "Max score is %i"


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
