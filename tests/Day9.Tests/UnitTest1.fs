module Day9.Tests

open NUnit.Framework
open Day9.Common

[<Test>]
let Test_MoveRight () =
    let initialState =
        { Head = Point.Zero
          Tail = Point.Zero
          History = [] }

    let moves = [ "R 2" ] |> List.fold Move.parseMove []

    let finalState = moves |> List.fold Move.handleMove initialState

    Assert.That(finalState.Head.X, Is.EqualTo(2))
    Assert.That(finalState.Head.Y, Is.EqualTo(0))
    Assert.That(finalState.Tail.X, Is.EqualTo(1))
    Assert.That(finalState.Tail.Y, Is.EqualTo(0))
    Assert.That(finalState.History, Is.EqualTo([ Point.Zero; { X = 1; Y = 0 } ]))

[<Test>]
let Test_MoveLeft () =
    let initialState =
        { Head = Point.Zero
          Tail = Point.Zero
          History = [] }

    let moves = [ "L 2" ] |> List.fold Move.parseMove []

    let finalState = moves |> List.fold Move.handleMove initialState

    Assert.That(finalState.Head.X, Is.EqualTo(-2))
    Assert.That(finalState.Head.Y, Is.EqualTo(0))
    Assert.That(finalState.Tail.X, Is.EqualTo(-1))
    Assert.That(finalState.Tail.Y, Is.EqualTo(0))
    Assert.That(finalState.History, Is.EqualTo([ Point.Zero; { X = -1; Y = 0 } ]))

[<Test>]
let Test_MoveUp () =
    let initialState =
        { Head = Point.Zero
          Tail = Point.Zero
          History = [] }

    let moves = [ "U 2" ] |> List.fold Move.parseMove []

    let finalState = moves |> List.fold Move.handleMove initialState

    Assert.That(finalState.Head.X, Is.EqualTo(0))
    Assert.That(finalState.Head.Y, Is.EqualTo(2))
    Assert.That(finalState.Tail.X, Is.EqualTo(0))
    Assert.That(finalState.Tail.Y, Is.EqualTo(1))
    Assert.That(finalState.History, Is.EqualTo([ Point.Zero; { X = 0; Y = 1 } ]))

[<Test>]
let Test_MoveDown () =
    let initialState =
        { Head = Point.Zero
          Tail = Point.Zero
          History = [] }

    let moves = [ "D 2" ] |> List.fold Move.parseMove []

    let finalState = moves |> List.fold Move.handleMove initialState

    Assert.That(finalState.Head.X, Is.EqualTo(0))
    Assert.That(finalState.Head.Y, Is.EqualTo(-2))
    Assert.That(finalState.Tail.X, Is.EqualTo(0))
    Assert.That(finalState.Tail.Y, Is.EqualTo(-1))
    Assert.That(finalState.History, Is.EqualTo([ Point.Zero; { X = 0; Y = -1 } ]))
