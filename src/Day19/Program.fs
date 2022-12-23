open System.IO
open Day19.Types
open Day19.Functions

let blueprints = ".\\test_data.txt" |> File.ReadLines |> Seq.map parseBlueprint

let mutable finalStates: State list = []

let limits = [
    { MaxOreRobots = 1
      MaxClayRobots = 1
      MaxObsidianRobots = 1
      MaxGeodeRobots = 1024  }
    { MaxOreRobots = 1
      MaxClayRobots = 2
      MaxObsidianRobots = 1024
      MaxGeodeRobots = 1024  }
    { MaxOreRobots = 1
      MaxClayRobots = 4
      MaxObsidianRobots = 1024
      MaxGeodeRobots = 1024  } ]
for limit in limits do
    let mutable stack = [ for blueprint in blueprints -> startSimulation 24 blueprint limit ]

    while stack.Length > 0 do
        let state = stack.Head
        stack <- stack.Tail

        if state.Time = state.MaxTime then
            if state.Resources.Geodes > 0 then
                finalStates <- finalStates |> List.append [ state ]
        else
            let newStates = state |> evaluateState
            stack <- newStates |> List.append stack

let sortedStates =
    finalStates
    |> List.where (fun s -> s.Resources.Geodes > 0)
    |> List.sortByDescending (fun s -> s.Resources.Geodes)
    
for s in sortedStates do
    s |> printState |> ignore

let rec computeQualityLevel (tuple: int*State list) : int =
    let id = fst tuple
    let maxGeodes =
        (snd tuple)
        |> List.map (fun s -> s.Resources.Geodes)
        |> List.max
    id * maxGeodes
    
let sum =
    finalStates
    |> List.groupBy (fun s -> s.Blueprint.Id)
    |> List.map computeQualityLevel
    |> List.sum
    
printfn "Sum %A" sum
