open System.IO

let instructions =
    ".\\part1_data.txt"
    |> File.ReadAllLines
    |> Seq.toList
    
type State = {
    Cycle : int
    X : int
    SignalStrengths: int list
}

let initialState = {
    Cycle = 1
    X = 1
    SignalStrengths = []
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
        
    let newSignalStrengths =
        if newCycle = 20 then
            List.append state.SignalStrengths [ newX * newCycle ]
        elif (newCycle - 20) % 40 = 0 then
            List.append state.SignalStrengths [ newX * newCycle ]
        else
            state.SignalStrengths
    
    {
        Cycle = newCycle
        X = newX
        SignalStrengths = newSignalStrengths
    }
            
    
let finalState = List.fold executeInstruction initialState simplifiedInstructions 

let sum = List.sum finalState.SignalStrengths

printfn "Sum: %i" sum

let x = 0