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

type Packetdata = | Integer of int | List of Packetdata list

let (|IntValue|_|) str =
    let m = Regex.Match(str, "^([0-9]+),?(.*)$")
    if m.Success then Some ((int m.Groups[1].Value), m.Groups[2].Value) else None

let (|ListValue|_|) (str: string) =
    if str[0] = '[' then
        let rec findClosure acc count (s:string) =
            if count = 0 then (acc |> Seq.tail |> rev |> (Array.ofSeq >> String)), s
            else
                match s[0] with
                | '[' -> findClosure ('['::acc) (count + 1) s[1..]
                | ']' -> findClosure (']'::acc) (count - 1) s[1..]
                | c -> findClosure (c::acc) count s[1..]
        findClosure [] 1 str[1..] |> Some
    else None
let (|Startswith|_|) c (str: string) = if str[0] = c then Some str[1..] else None

let parsePacket (str: string) : Packetdata =
    let rec parse (acc:Packetdata list) s =
        //Console.WriteLine $"Parsing \"{s}\" acc is {acc}"
        if s = "" then rev acc
        else
            match s with
            | IntValue (i,rest) -> parse ((Integer i)::acc) rest
            | ListValue (s,rest) ->
                let listValue = List (parse [] s)
                parse (listValue::acc) rest
            | Startswith ',' rest -> parse acc rest
            | other -> failwith $"Unexpected input {other}"
    parse [] str |> function [x] -> x | _ -> failwith "expected single packet"
    
type OrderResult = | Order | Mess | Equal

let rec isOrdered p1 p2 =
    match (p1, p2) with
    | (Integer i1, Integer i2) -> if p1 < p2 then Order elif p1 > p2 then Mess else Equal
    | (List l1, List l2) ->
        match (l1, l2) with
        | (h1::t1, h2::t2) ->
            match isOrdered h1 h2 with
                | Order -> Order
                | Mess -> Mess
                | Equal -> isOrdered (List t1) (List t2)
        | ([], h2::t2) -> Order
        | (h1::t1, []) -> Mess
        | ([], []) -> Equal
    | (Integer i1, List l2) -> isOrdered (List [Integer i1]) (List l2)
    | (List l1, Integer i2) -> isOrdered (List l1) (List [Integer i2])

let parseInput strings =
    strings
    |> Seq.split [[""]]
    |>> Seq.toList
    |>> List.map parsePacket

let part1 =
    fileName
    >>= readFile
    |>> parseInput
    |>> (Seq.map (fun (pair: Packetdata list) -> isOrdered pair[0] pair[1]))
    |>> Seq.mapi (fun i res -> match res with | Order -> Some (i + 1) | _ -> None)
    |>> Seq.choose id
    |>> Seq.sum
    |>> sprintf "Sum is %i"

let part2 = monad {
    let! input = fileName >>= readFile |>> (Seq.where (fun s -> s <> ""))
    let standardPackets = input |>> parsePacket |>> (fun p -> false, p) |> List.ofSeq
    let tracerPackets = [|"[[2]]";"[[6]]"|] |>> parsePacket |>> (fun p -> true, p) |> List.ofSeq
    let packets = 
        tracerPackets @ standardPackets
        |> List.sortWith (fun (_, p1) (_, p2) -> isOrdered p1 p2 |> function | Equal -> 0 | Order -> -1 | Mess -> 1)
    let! key = 
        packets
        |>> fst
        |> List.mapi (fun i res -> if res then Some (i + 1) else None)
        |> List.choose id
        |> function [m1;m2] -> Ok (m1 * m2) | _ -> Error "Not two markers"
    return $"Key is {key}"
}

module Tests =
    let testParsePacket str packet =
        fun () -> 
            parsePacket str |> fun p -> if p = packet then Ok () else Error $"Error parsing {str}. Expected {packet}, was {p}"
    let private tests =
        [
            fun () -> "[10]" |> function ListValue ("10", "") -> Ok () | ListValue other -> Error $"Was {other}"| other -> Error other
            fun () -> "[1,2],3" |> function ListValue ("1,2", ",3") -> Ok () | ListValue other -> Error $"Was {other}"| other -> Error other
            testParsePacket "[10]" (List [(Integer 10)])
            testParsePacket "[1,2,3]" (List ([1;2;3] |>> Integer))
            testParsePacket "[1,[2,3]]" (List [Integer 1; List ([2;3] |>> Integer)])
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
