#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus
open System.Text.RegularExpressions

type File = { Name: string; Size: int64}

type CdCommand =
    | Up
    | Root
    | Down of string
type Command =
    | Cd of CdCommand
    | Ls

type Input =
    | Command of Command
    | Directory of string
    | File of File

type DirectoryEntry =
    | File of int64
    | Directory of string list

let (|Cd|_|) str =
    let m = Regex.Match(str, "\$ cd (/|\.\.|[a-zA-Z0-9]*)")
    if m.Success then 
        m.Groups[1].Value |> function
            | "/" -> Root
            | ".." -> Up
            | s -> Down s 
        |> Command.Cd |> Some 
    else None

let (|Dir|_|) str =
    let m = Regex.Match(str, "dir ([a-zA-Z0-9]*)")
    if m.Success then m.Groups[1].Value |> Input.Directory |> Some else None

let (|Ls|_|) str =
    if str = "$ ls" then Command.Ls |> Some else None

let (|File|_|) str =
    let m = Regex.Match(str, "([0-9]*) ([a-zA-Z0-9\.]*)")
    if m.Success then { Name = m.Groups[2].Value ; Size = int m.Groups[1].Value } |> Input.File |> Some else None

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

let parseLine = function
    | Cd x
    | Ls x -> x |> Command
    | Dir x
    | File x -> x
    | other -> failwith $"Unexpected input {other}"

let parseInput input =
    input
    |> Seq.map parseLine
    |> Seq.fold
        (fun ((directories: Map<string list, Map<string, DirectoryEntry>>), path) value ->
            match value with
                | Command Command.Ls -> (directories, path)
                | Command (Command.Cd Root) -> (directories, [])
                | Command (Command.Cd Up) -> (directories, path |> List.tail )
                | Command (Command.Cd (Down s)) -> (directories, s::path )
                | Input.Directory s -> (
                    let key = s::path
                    let dir = directories[path] |> Map.add s (DirectoryEntry.Directory key)
                    directories |> Map.containsKey key |> function
                    | true -> ( Map.add path dir directories, path)
                    | false -> ( Map.add path dir directories |> Map.add key Map.empty, path)
                    )
                | Input.File f -> (
                    directories[path] |> Map.add f.Name (DirectoryEntry.File f.Size)
                    |> fun dir -> Map.add path dir directories
                    , path )
        )
        (Map.empty |> Map.add [] Map.empty, [])
    |> fst

let rec directorySize (tree:Map<string list, Map<string, DirectoryEntry>>) path : int64 =
    tree[path] |> Map.toSeq |>> snd 
    |> Seq.sumBy (function
        | DirectoryEntry.File i -> i
        | DirectoryEntry.Directory s -> directorySize tree s
        )
    
module Tests =
    let private tests = 
        [
            fun () -> "14848514 b.txt" |> parseLine |> function
                | Input.File {Name = "b.txt" ; Size = 14848514L } -> Ok () 
                | other -> Error $"{other}"
            fun () -> "$ cd /" |> parseLine |> function
                | Input.Command (Command.Cd Root) -> Ok () 
                | other -> Error $"{other}"
        ]
    let run () =
        tests 
        |> List.fold (fun state test -> state |> Result.bind test ) (Ok ())
        |>> fun () -> "All tests Ok"


let part1 =
    fileName
    >>= readFile
    |>> parseInput
    |>> (fun tree ->
        tree
        |> Map.toSeq
        |>> fst
        |>> directorySize tree)
    |>> (Seq.where (fun i -> i <= 100000L))
    |>> Seq.sum
    |>> sprintf "Sum is %i"

let part2 =
    monad {
        let! tree =
            fileName
            >>= readFile
            |>> parseInput
        let sizeToDelete = (directorySize tree []) - 70000000L + 30000000L
        let directorySizes = 
            tree
            |> Map.toSeq
            |>> fst
            |>> directorySize tree
        return directorySizes |> Seq.where (fun i -> i >= sizeToDelete) |> Seq.min
    }
    |>> sprintf "Answer is %i"


Result.map3
    (sprintf "Successful run!\r\nTests: %s\r\nPart1: %s\r\nPart2: %s")
    (Tests.run ())
    part1
    part2
|> function
| Ok s -> s
| Error s -> sprintf "Error: %s" s 
|> Console.WriteLine 
