open System
open System.IO

let data =
    File.ReadLines ".\part1_data.txt" |> List.ofSeq

let index =
    List.findIndex String.IsNullOrWhiteSpace data

let stackLines = data[.. (index - 2)]
List.iter (fun l -> printfn "%s" l) stackLines
let moveLines = data[(index + 1) ..]
List.iter (fun l -> printfn "%s" l) moveLines

let numberOfStacks = ((List.last stackLines).Split [| ' ' |] ).Length

printfn "Stacks: %i" numberOfStacks

let stacks: string list list =
    [ for i in 1..numberOfStacks -> [] ]
    
let updateStack (index: int) (line: string) (stack: string list) =
    let column = 4 * index + 1
    if line[column] = ' ' then
        stack
    else
        List.append [ line[column] |> string ] stack

let parseStackLine (line: string) (acc: string list list) =
    [ for i in 0..(numberOfStacks-1) -> updateStack i line acc[i]]
    
let stackMap = List.foldBack parseStackLine stackLines stacks

type Move = {
    Source: int
    Target: int
    Offset: int
}

let expandMoveLine (acc: Move list) (line : string) =
    let tokens = line.Split [| ' ' |]
    let count = tokens[1] |> Int32.Parse
    let source = tokens[3] |> Int32.Parse
    let target = tokens[5] |> Int32.Parse
    List.append acc [for i in 1..count -> {Source = source; Target = target; Offset = count - i}]
    
let moves = List.fold expandMoveLine [] moveLines

let moveStack (acc: string list list) (move: Move) = 
    let sourceStack = acc[move.Source - 1]
    let crate = sourceStack[move.Offset]
    let newSourceStack = List.removeAt move.Offset sourceStack
    let targetStack = acc[move.Target - 1]
    let newTargetStack = List.append [ crate ] targetStack
    [for i in 1..acc.Length ->
        if i = move.Source then
            newSourceStack
        elif i = move.Target then
            newTargetStack
        else
            acc[i - 1]]
    
let topStacks = List.fold moveStack stackMap moves |> List.fold (fun acc elem -> acc + elem[0]) ""

printfn "Top Stack: %s" topStacks