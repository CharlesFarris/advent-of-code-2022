module Day9.Tests

open NUnit.Framework
open Day9.Common

[<Test>]
let Test_MoveRight () =
    let initialState =
        { Knots = [ Point.Zero; Point.Zero ]
          History = [] }

    let moves = [ "R 2" ] |> List.fold Move.parseMove []

    let finalState = moves |> List.fold Move.handleMove initialState

    Assert.That(finalState.Knots, Is.EqualTo([ { X = 2; Y = 0 }; { X = 1; Y = 0 } ]))
    Assert.That(finalState.History, Is.EqualTo([ Point.Zero; { X = 1; Y = 0 } ]))

[<Test>]
let Test_MoveLeft () =
    let initialState =
        { Knots = [ Point.Zero; Point.Zero ]
          History = [] }

    let moves = [ "L 2" ] |> List.fold Move.parseMove []

    let finalState = moves |> List.fold Move.handleMove initialState

    Assert.That(finalState.Knots, Is.EqualTo([ { X = -2; Y = 0 }; { X = -1; Y = 0 } ]))
    Assert.That(finalState.History, Is.EqualTo([ Point.Zero; { X = -1; Y = 0 } ]))

[<Test>]
let Test_MoveUp () =
    let initialState =
        { Knots = [ Point.Zero; Point.Zero ]
          History = [] }

    let moves = [ "U 2" ] |> List.fold Move.parseMove []

    let finalState = moves |> List.fold Move.handleMove initialState

    Assert.That(finalState.Knots, Is.EqualTo([ { X = 0; Y = 2 }; { X = 0; Y = 1 } ]))
    Assert.That(finalState.History, Is.EqualTo([ Point.Zero; { X = 0; Y = 1 } ]))

[<Test>]
let Test_MoveDown () =
    let initialState =
        { Knots = [ Point.Zero; Point.Zero; Point.Zero ]
          History = [] }

    let moves = [ "D 4"; "U 4" ] |> List.fold Move.parseMove []

    let finalState = moves |> List.fold Move.handleMove initialState

    Assert.That(finalState.Knots, Is.EqualTo([ { X = 0; Y = -4 }; { X = 0; Y = -3 }; { X = 0; Y = -2 } ]))
//Assert.That(finalState.History, Is.EqualTo([ Point.Zero; { X = 0; Y = -1 } ]))
