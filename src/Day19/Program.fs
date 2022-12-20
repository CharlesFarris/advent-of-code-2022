﻿open System.IO

type OreRobotBlueprint = { Ore: int }

type ClayRobotBlueprint = { Ore: int }

type ObsidianRobotBlueprint = { Ore: int; Clay: int }

type GeodeRobotBlueprint = { Ore: int; Obsidian: int }

type Blueprint =
    { Id: int
      OreRobotBlueprint: OreRobotBlueprint
      ClayRobotBlueprint: ClayRobotBlueprint
      ObsidianRobotBlueprint: ObsidianRobotBlueprint
      GeodeRobotBlueprint: GeodeRobotBlueprint }

let parseBlueprint (line: string) : Blueprint =
    let tokens = line.Split [| ' '; ':' |]

    { Id = int tokens[1]
      OreRobotBlueprint = { Ore = int tokens[7] }
      ClayRobotBlueprint = { Ore = int tokens[13] }
      ObsidianRobotBlueprint =
        { Ore = int tokens[19]
          Clay = int tokens[22] }
      GeodeRobotBlueprint =
        { Ore = int tokens[28]
          Obsidian = int tokens[31] } }

type RobotFactoryRequest =
    | None
    | OreRobot
    | ClayRobot
    | ObsidianRobot
    | GeodeRobot

type Resources =
    { Ore: int
      Clay: int
      Obsidian: int
      Geodes: int }

type Robots =
    { OreRobots: int
      ClayRobots: int
      ObsidianRobots: int
      GeodeRobots: int }

let collectResources (robots: Robots) (resources: Resources) : Resources =
    { Ore = resources.Ore + robots.OreRobots
      Clay = resources.Clay + robots.ClayRobots
      Obsidian = resources.Obsidian + robots.ObsidianRobots
      Geodes = resources.Geodes + robots.GeodeRobots }

let buyRobot (blueprint: Blueprint) (request: RobotFactoryRequest) (resources: Resources) : Resources =
    match request with
    | None -> resources
    | OreRobot -> { resources with Ore = resources.Ore - blueprint.OreRobotBlueprint.Ore }
    | ClayRobot -> { resources with Ore = resources.Ore - blueprint.ClayRobotBlueprint.Ore }
    | ObsidianRobot ->
        { resources with
            Ore = resources.Ore - blueprint.ObsidianRobotBlueprint.Ore
            Clay = resources.Clay - blueprint.ObsidianRobotBlueprint.Clay }
    | GeodeRobot ->
        { resources with
            Ore = resources.Ore - blueprint.GeodeRobotBlueprint.Ore
            Obsidian = resources.Obsidian - blueprint.GeodeRobotBlueprint.Obsidian }

let buildRobot (request: RobotFactoryRequest) (robots: Robots) : Robots =
    match request with
    | None -> robots
    | OreRobot -> { robots with OreRobots = robots.OreRobots + 1 }
    | ClayRobot -> { robots with ClayRobots = robots.ClayRobots + 1 }
    | ObsidianRobot -> { robots with ObsidianRobots = robots.ObsidianRobots + 1 }
    | GeodeRobot -> { robots with GeodeRobots = robots.GeodeRobots + 1 }

type State =
    { Time: int
      Resources: Resources
      Robots: Robots
      MaxTime: int
      Blueprint: Blueprint }

let printState (state: State) : State =
    printfn
        "Blueprint: %2i Time: %2i Resources: %2i %2i %2i %2i Robots:%2i %2i %2i %2i"
        state.Blueprint.Id
        state.Time
        state.Resources.Ore
        state.Resources.Clay
        state.Resources.Obsidian
        state.Resources.Geodes
        state.Robots.OreRobots
        state.Robots.ClayRobots
        state.Robots.ObsidianRobots
        state.Robots.GeodeRobots

    state

let canBuildOreRobot (state: State) : bool =
    state.Resources.Ore >= state.Blueprint.OreRobotBlueprint.Ore

let canBuildClayRobot (state: State) : bool =
    state.Resources.Ore >= state.Blueprint.ClayRobotBlueprint.Ore

let canBuildObsidianRobot (state: State) : bool =
    state.Resources.Ore >= state.Blueprint.ObsidianRobotBlueprint.Ore
    && state.Resources.Clay >= state.Blueprint.ObsidianRobotBlueprint.Clay

let canBuildGeodeRobot (state: State) : bool =
    state.Resources.Ore >= state.Blueprint.GeodeRobotBlueprint.Ore
    && state.Resources.Obsidian >= state.Blueprint.GeodeRobotBlueprint.Obsidian

let runSimulation (request: RobotFactoryRequest) (state: State) : State =
    let newTime = state.Time + 1

    let newResources =
        state.Resources
        |> buyRobot state.Blueprint request
        |> collectResources state.Robots

    let newRobots = state.Robots |> buildRobot request

    let newState =
        { state with
            Time = newTime
            Robots = newRobots
            Resources = newResources }

    newState |> printState

let startSimulation (maxTime: int) (blueprint: Blueprint) : State =
    { Blueprint = blueprint
      Time = 0
      MaxTime = maxTime

      Robots =
          { OreRobots = 1
            ClayRobots = 0
            ObsidianRobots = 0
            GeodeRobots = 0 }

      Resources =
          { Ore = 0
            Clay = 0
            Obsidian = 0
            Geodes = 0 } }

let evaluateState (state: State) : State list =
    //state |> printState |> ignore
    let mutable states = []

    if state |> canBuildGeodeRobot then
        states <- [ state |> runSimulation GeodeRobot ] |> List.append states
        states <- [ state |> runSimulation ObsidianRobot ] |> List.append states

    elif state |> canBuildObsidianRobot then
        states <- [ state |> runSimulation ObsidianRobot ] |> List.append states
        states <- [ state |> runSimulation ClayRobot ] |> List.append states

    elif state |> canBuildClayRobot then
        states <- [ state |> runSimulation ClayRobot ] |> List.append states

    elif state |> canBuildOreRobot then
        states <- [ state |> runSimulation OreRobot ] |> List.append states

    else
        states <- [ state |> runSimulation None ] |> List.append states

    states

let blueprints = ".\\test_data.txt" |> File.ReadLines |> Seq.map parseBlueprint

let mutable stack = [ for blueprint in blueprints -> startSimulation 24 blueprint ]
let mutable finalStates: State list = []

while stack.Length > 0 do
    let state = stack.Head
    stack <- stack.Tail

    if state.Time = state.MaxTime then
        if state.Resources.Geodes > 0 then
            finalStates <- finalStates |> List.append [ state ]
    else
        let newStates = state |> evaluateState
        stack <- newStates |> List.append stack

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
