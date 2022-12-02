
// let strategyGuide : string list = [ "A Y"; "B X"; "C Z" ]

open System.IO

let strategyGuide =
    File.ReadLines ".\part1_data.txt" |> List.ofSeq

let playRound (score: int) (line: string): int =
    match line with
    | "A X" -> score + 3 + 0 // rock-loss -> scissor
    | "A Y" -> score + 1 + 3 // rock-draw -> rock
    | "A Z" -> score + 2 + 6 // rock-win -> paper
    | "B X" -> score + 1 + 0 // paper-loss -> rock
    | "B Y" -> score + 2 + 3 // paper-draw -> paper
    | "B Z" -> score + 3 + 6 // paper-win -> scissor 
    | "C X" -> score + 2 + 0 // scissors-loss -> paper 
    | "C Y" -> score + 3 + 3 // scissors-draw -> scissors
    | "C Z" -> score + 1 + 6 // scissors-win -> rock
    | _ -> score
    
let score = List.fold playRound 0 strategyGuide

printfn "%i" score

   