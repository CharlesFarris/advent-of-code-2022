open System.IO
open Range

let instructions =
    ".\\part1_data.txt"
    |> File.ReadAllLines
    |> Seq.toList
    
type State = {
    Cycle : int
    X : int
    Current: string
    Lines: string list
}

let initialState = {
    Cycle = 1
    X = 1
    Current = ""
    Lines = []
}

let simplifyInstruction (acc: string list) (line: string) : string list =
    let tokens = line.Split [| ' ' |]
    match tokens[0] with
    | "addx" -> List.append acc [ "noop"; (sprintf "add %s" tokens[1]) ]
    | "noop" -> List.append acc [ "noop" ]
    | _ -> invalidOp tokens[0]
    
let simplifiedInstructions = List.fold simplifyInstruction [] instructions

let executeInstruction (state: State) (line: string) : State =
    let tokens = line.Split [| ' ' |]
    let newCycle = state.Cycle + 1
    let newX = match tokens[0] with
                | "noop" -> state.X
                | "add" -> state.X + int tokens[1]
                | _ -> invalidOp tokens[0]
    let sprite = Range1d.create (state.X - 1) (state.X + 1)
    let pixel = state.Current.Length
    let newCurrent =
        if Range1d.containsValue pixel sprite then
            state.Current + "#"
        else
            state.Current + "."
    if newCurrent.Length = 40 then
        {
            X = newX
            Cycle = newCycle
            Current = ""
            Lines = List.append state.Lines [ newCurrent ]
        }
    else
        {
            X = newX
            Cycle = newCycle
            Current = newCurrent
            Lines = state.Lines
        }
    
let finalState = List.fold executeInstruction initialState simplifiedInstructions 

List.iter (fun line -> printfn "%s" line) finalState.Lines

let x = 0