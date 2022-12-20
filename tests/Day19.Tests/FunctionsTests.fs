module Day19.Tests

open Day19.Types
open NUnit.Framework
open Day19.Functions

[<Test>]
let runSimulation_ValidatesBehavior () =
    let lines = [ "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian." ]
    let blueprints =lines|> List.map parseBlueprint
    
    Assert.That(blueprints[0].ClayRobotBlueprint.Ore, Is.EqualTo(2))
    Assert.That(blueprints[0].GeodeRobotBlueprint.Obsidian, Is.EqualTo(7))
    Assert.That(blueprints[0].GeodeRobotBlueprint.Ore, Is.EqualTo(2))
    Assert.That(blueprints[0].ObsidianRobotBlueprint.Ore, Is.EqualTo(3))
    Assert.That(blueprints[0].ObsidianRobotBlueprint.Clay, Is.EqualTo(14))
    Assert.That(blueprints[0].OreRobotBlueprint.Ore, Is.EqualTo(4))
    
    let state0 = startSimulation 24 blueprints[0]
    Assert.That(state0.Blueprint, Is.EqualTo(blueprints[0]))
    Assert.That(state0.MaxTime, Is.EqualTo(24))
    Assert.That(state0.Time, Is.EqualTo(0))
    Assert.That(state0.Resources.Clay, Is.EqualTo(0))
    Assert.That(state0.Resources.Geodes, Is.EqualTo(0))
    Assert.That(state0.Resources.Obsidian, Is.EqualTo(0))
    Assert.That(state0.Resources.Ore, Is.EqualTo(0))
    Assert.That(state0.Robots.ClayRobots, Is.EqualTo(0))
    Assert.That(state0.Robots.GeodeRobots, Is.EqualTo(0))
    Assert.That(state0.Robots.ObsidianRobots, Is.EqualTo(0))
    Assert.That(state0.Robots.OreRobots, Is.EqualTo(1))

    let state1 = state0 |> runSimulation NoRequest
    Assert.That(state1.Blueprint, Is.EqualTo(blueprints[0]))
    Assert.That(state1.MaxTime, Is.EqualTo(24))
    Assert.That(state1.Time, Is.EqualTo(1))
    Assert.That(state1.Resources.Clay, Is.EqualTo(0))
    Assert.That(state1.Resources.Geodes, Is.EqualTo(0))
    Assert.That(state1.Resources.Obsidian, Is.EqualTo(0))
    Assert.That(state1.Resources.Ore, Is.EqualTo(1))
    Assert.That(state1.Robots.ClayRobots, Is.EqualTo(0))
    Assert.That(state1.Robots.GeodeRobots, Is.EqualTo(0))
    Assert.That(state1.Robots.ObsidianRobots, Is.EqualTo(0))
    Assert.That(state1.Robots.OreRobots, Is.EqualTo(1))
    
    let state2 = state1 |> runSimulation NoRequest
    Assert.That(state2.Blueprint, Is.EqualTo(blueprints[0]))
    Assert.That(state2.MaxTime, Is.EqualTo(24))
    Assert.That(state2.Time, Is.EqualTo(2))
    Assert.That(state2.Resources.Clay, Is.EqualTo(0))
    Assert.That(state2.Resources.Geodes, Is.EqualTo(0))
    Assert.That(state2.Resources.Obsidian, Is.EqualTo(0))
    Assert.That(state2.Resources.Ore, Is.EqualTo(2))
    Assert.That(state2.Robots.ClayRobots, Is.EqualTo(0))
    Assert.That(state2.Robots.GeodeRobots, Is.EqualTo(0))
    Assert.That(state2.Robots.ObsidianRobots, Is.EqualTo(0))
    Assert.That(state2.Robots.OreRobots, Is.EqualTo(1))
    
    let state3 = state2 |> runSimulation ClayRobot
    Assert.That(state3.Blueprint, Is.EqualTo(blueprints[0]))
    Assert.That(state3.MaxTime, Is.EqualTo(24))
    Assert.That(state3.Time, Is.EqualTo(3))
    Assert.That(state3.Resources.Clay, Is.EqualTo(0))
    Assert.That(state3.Resources.Geodes, Is.EqualTo(0))
    Assert.That(state3.Resources.Obsidian, Is.EqualTo(0))
    Assert.That(state3.Resources.Ore, Is.EqualTo(1))
    Assert.That(state3.Robots.ClayRobots, Is.EqualTo(1))
    Assert.That(state3.Robots.GeodeRobots, Is.EqualTo(0))
    Assert.That(state3.Robots.ObsidianRobots, Is.EqualTo(0))
    Assert.That(state3.Robots.OreRobots, Is.EqualTo(1))
            
    let state4 = state3 |> runSimulation NoRequest
    Assert.That(state4.Blueprint, Is.EqualTo(blueprints[0]))
    Assert.That(state4.MaxTime, Is.EqualTo(24))
    Assert.That(state4.Time, Is.EqualTo(4))
    Assert.That(state4.Resources.Clay, Is.EqualTo(1))
    Assert.That(state4.Resources.Geodes, Is.EqualTo(0))
    Assert.That(state4.Resources.Obsidian, Is.EqualTo(0))
    Assert.That(state4.Resources.Ore, Is.EqualTo(2))
    Assert.That(state4.Robots.ClayRobots, Is.EqualTo(1))
    Assert.That(state4.Robots.GeodeRobots, Is.EqualTo(0))
    Assert.That(state4.Robots.ObsidianRobots, Is.EqualTo(0))
    Assert.That(state4.Robots.OreRobots, Is.EqualTo(1))

    let state5 = state4 |> runSimulation ClayRobot
    Assert.That(state5.Blueprint, Is.EqualTo(blueprints[0]))
    Assert.That(state5.MaxTime, Is.EqualTo(24))
    Assert.That(state5.Time, Is.EqualTo(5))
    Assert.That(state5.Resources.Clay, Is.EqualTo(2))
    Assert.That(state5.Resources.Geodes, Is.EqualTo(0))
    Assert.That(state5.Resources.Obsidian, Is.EqualTo(0))
    Assert.That(state5.Resources.Ore, Is.EqualTo(1))
    Assert.That(state5.Robots.ClayRobots, Is.EqualTo(2))
    Assert.That(state5.Robots.GeodeRobots, Is.EqualTo(0))
    Assert.That(state5.Robots.ObsidianRobots, Is.EqualTo(0))
    Assert.That(state5.Robots.OreRobots, Is.EqualTo(1))

    let state6 = state5 |> runSimulation NoRequest
    Assert.That(state6.Blueprint, Is.EqualTo(blueprints[0]))
    Assert.That(state6.MaxTime, Is.EqualTo(24))
    Assert.That(state6.Time, Is.EqualTo(6))
    Assert.That(state6.Resources.Clay, Is.EqualTo(4))
    Assert.That(state6.Resources.Geodes, Is.EqualTo(0))
    Assert.That(state6.Resources.Obsidian, Is.EqualTo(0))
    Assert.That(state6.Resources.Ore, Is.EqualTo(2))
    Assert.That(state6.Robots.ClayRobots, Is.EqualTo(2))
    Assert.That(state6.Robots.GeodeRobots, Is.EqualTo(0))
    Assert.That(state6.Robots.ObsidianRobots, Is.EqualTo(0))
    Assert.That(state6.Robots.OreRobots, Is.EqualTo(1))

    let state7 = state6 |> runSimulation ClayRobot
    Assert.That(state7.Blueprint, Is.EqualTo(blueprints[0]))
    Assert.That(state7.MaxTime, Is.EqualTo(24))
    Assert.That(state7.Time, Is.EqualTo(7))
    Assert.That(state7.Resources.Clay, Is.EqualTo(6))
    Assert.That(state7.Resources.Geodes, Is.EqualTo(0))
    Assert.That(state7.Resources.Obsidian, Is.EqualTo(0))
    Assert.That(state7.Resources.Ore, Is.EqualTo(1))
    Assert.That(state7.Robots.ClayRobots, Is.EqualTo(3))
    Assert.That(state7.Robots.GeodeRobots, Is.EqualTo(0))
    Assert.That(state7.Robots.ObsidianRobots, Is.EqualTo(0))
    Assert.That(state7.Robots.OreRobots, Is.EqualTo(1))
    
    let state8 = state7 |> runSimulation NoRequest
    let state9 = state8 |> runSimulation NoRequest
    let state10 = state9 |> runSimulation NoRequest

    Assert.That(state10.Blueprint, Is.EqualTo(blueprints[0]))
    Assert.That(state10.MaxTime, Is.EqualTo(24))
    Assert.That(state10.Time, Is.EqualTo(10))
    Assert.That(state10.Resources.Clay, Is.EqualTo(15))
    Assert.That(state10.Resources.Geodes, Is.EqualTo(0))
    Assert.That(state10.Resources.Obsidian, Is.EqualTo(0))
    Assert.That(state10.Resources.Ore, Is.EqualTo(4))
    Assert.That(state10.Robots.ClayRobots, Is.EqualTo(3))
    Assert.That(state10.Robots.GeodeRobots, Is.EqualTo(0))
    Assert.That(state10.Robots.ObsidianRobots, Is.EqualTo(0))
    Assert.That(state10.Robots.OreRobots, Is.EqualTo(1))
    
    let state11 = state10 |> runSimulation ObsidianRobot
    let state12 = state11 |> runSimulation ClayRobot
    let state13 = state12 |> runSimulation NoRequest
    let state14 = state13 |> runSimulation NoRequest
    let state15 = state14 |> runSimulation ObsidianRobot
    let state16 = state15 |> runSimulation NoRequest
    let state17 = state16 |> runSimulation NoRequest
    let state18 = state17 |> runSimulation GeodeRobot
    let state19 = state18 |> runSimulation NoRequest
    let state20 = state19 |> runSimulation NoRequest
    let state21 = state20 |> runSimulation GeodeRobot
    let state22 = state21 |> runSimulation NoRequest
    let state23 = state22 |> runSimulation NoRequest
    let state24 = state23 |> runSimulation NoRequest
    
    Assert.That(state24.Blueprint, Is.EqualTo(blueprints[0]))
    Assert.That(state24.MaxTime, Is.EqualTo(24))
    Assert.That(state24.Time, Is.EqualTo(24))
    Assert.That(state24.Resources.Clay, Is.EqualTo(41))
    Assert.That(state24.Resources.Geodes, Is.EqualTo(9))
    Assert.That(state24.Resources.Obsidian, Is.EqualTo(8))
    Assert.That(state24.Resources.Ore, Is.EqualTo(6))
    Assert.That(state24.Robots.ClayRobots, Is.EqualTo(4))
    Assert.That(state24.Robots.GeodeRobots, Is.EqualTo(2))
    Assert.That(state24.Robots.ObsidianRobots, Is.EqualTo(2))
    Assert.That(state24.Robots.OreRobots, Is.EqualTo(1))
    
