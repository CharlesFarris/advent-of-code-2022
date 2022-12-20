namespace Day19.Types

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

type RobotFactoryRequest =
    | NoRequest
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

type State =
    { Time: int
      Resources: Resources
      Robots: Robots
      MaxTime: int
      Blueprint: Blueprint }

