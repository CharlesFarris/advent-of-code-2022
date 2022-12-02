
// let strategyGuide : string list = [ "A Y"; "B X"; "C Z" ]

open System.IO

let strategyGuide =
    File.ReadLines ".\part1_data.txt" |> List.ofSeq

let playRound (score: int) (line: string): int =
    match line with
    | "A X" -> score + 1 + 3 // rock-rock 
    | "A Y" -> score + 2 + 6 // rock-paper
    | "A Z" -> score + 3 + 0 // rock-scissors
    | "B X" -> score + 1 + 0 // paper-rock
    | "B Y" -> score + 2 + 3 // paper-paper
    | "B Z" -> score + 3 + 6 // paper-scissors
    | "C X" -> score + 1 + 6 // scissors-rock
    | "C Y" -> score + 2 + 0 // scissors-paper
    | "C Z" -> score + 3 + 3 // scissors-scissors
    | _ -> score
    
let score = List.fold playRound 0 strategyGuide

printfn "%i" score

   