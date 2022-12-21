open System.IO
open Day21
open Day21.Types
open Functions

let initialState =
    ".\\part1_data.txt"
    |> File.ReadAllLines
    |> Seq.fold parseLine { Numbers = Map.empty; Jobs = [] }

printfn "%A" initialState

let finalState = initialState |> runSimulation

printfn "%A" finalState

let root = finalState.Numbers |> Map.find "root"
printfn "Root: %i" root
