open System
open System.IO
    
let rucksacks =
    File.ReadLines ".\part1_data.txt" |> List.ofSeq
    
let groups = rucksacks |> Seq.chunkBySize 3

let computePriority (c: char) : int =
    if ('a' <= c && c <= 'z') then
        int c - 97 + 1
    elif ('A' <= c && c <= 'Z') then
        int c - 65 + 27
    else
        0

let findBadgePriority (lines : string[]) : int =
    let matchesToSecond = String.filter lines[1].Contains lines[0] |> Seq.toList |> List.distinct |> Array.ofSeq |> String
    let matchesToThird = String.filter lines[2].Contains matchesToSecond |> Seq.toList |> List.distinct
    matchesToThird[0] |> computePriority

let sum =
    List.fold (fun sum group -> sum + findBadgePriority group) 0 (groups |> Seq.toList)

printfn "Sum: %i" sum
