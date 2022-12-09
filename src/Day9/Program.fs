open System.IO
open Day9.Common

let moves =
    ".\\part1_data.txt"
    |> File.ReadAllLines
    |> Seq.toList
    |> List.fold Move.parseMove []


let initialState =
    { Head = Point.Zero 
      Tail = Point.Zero
      History = [ Point.Zero ] }

let finalState = moves |> List.fold Move.handleMove initialState

let distinctHistory = finalState.History |> List.distinct

printfn "Count: %i" finalState.History.Length
printfn "Distinct Count: %i" distinctHistory.Length
