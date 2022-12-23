open System.IO

let initialState =
    ".\\test_data.txt"
    |> File.ReadAllLines
    |> Seq.toList
    |> Day22.Functions.parseLines

initialState |> Day22.Functions.printState

let finalState = initialState |> Day22.Functions.runSimulation

finalState |> Day22.Functions.printState

printfn
    "Password: %i"
    ((1000 * finalState.Cursor.Y)
     + (4 * finalState.Cursor.X)
     + (finalState.Facing |> Day22.Functions.computeFacingValue))
