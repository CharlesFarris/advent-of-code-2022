module Day19.Functions

open Day19.Types

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

let collectResources (robots: Robots) (resources: Resources) : Resources =
    { Ore = resources.Ore + robots.OreRobots
      Clay = resources.Clay + robots.ClayRobots
      Obsidian = resources.Obsidian + robots.ObsidianRobots
      Geodes = resources.Geodes + robots.GeodeRobots }

let buyRobot (blueprint: Blueprint) (request: RobotFactoryRequest) (resources: Resources) : Resources =
    let newResources =
        match request with
        | NoRequest -> resources
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
    if newResources.Ore < 0 || newResources.Clay < 0 || newResources.Obsidian < 0 || newResources.Geodes < 0 then
        invalidOp "bad buy"
    newResources

let buildRobot (request: RobotFactoryRequest) (robots: Robots) : Robots =
    match request with
    | NoRequest -> robots
    | OreRobot -> { robots with OreRobots = robots.OreRobots + 1 }
    | ClayRobot -> { robots with ClayRobots = robots.ClayRobots + 1 }
    | ObsidianRobot -> { robots with ObsidianRobots = robots.ObsidianRobots + 1 }
    | GeodeRobot -> { robots with GeodeRobots = robots.GeodeRobots + 1 }


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
    
let createRobotConstraint (maxOreRobots: int) (maxClaRobots: int) (maxObsidianRobots: int) (maxGeodeRobots: int) : RobotLimits =
    {
        MaxOreRobots = maxOreRobots
        MaxClayRobots = maxClaRobots
        MaxObsidianRobots = maxObsidianRobots
        MaxGeodeRobots = maxGeodeRobots
    }

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

    newState //|> printState

let startSimulation (maxTime: int) (blueprint: Blueprint) (limits: RobotLimits): State =
    { Blueprint = blueprint
      Time = 0
      MaxTime = maxTime
      Limits = limits

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

    if state |> canBuildGeodeRobot && state.Robots.GeodeRobots < state.Limits.MaxGeodeRobots then
        states <- [ state |> runSimulation GeodeRobot ] |> List.append states

    if state |> canBuildObsidianRobot && state.Robots.ObsidianRobots < state.Limits.MaxObsidianRobots then
        states <- [ state |> runSimulation ObsidianRobot ] |> List.append states

    if state |> canBuildClayRobot && state.Robots.ClayRobots < state.Limits.MaxClayRobots then
        states <- [ state |> runSimulation ClayRobot ] |> List.append states

    if state |> canBuildOreRobot && state.Robots.OreRobots < state.Limits.MaxOreRobots then
        states <- [ state |> runSimulation OreRobot ] |> List.append states

    if states.IsEmpty then
        states <- [ state |> runSimulation NoRequest ] |> List.append states

    states