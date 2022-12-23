module Day22.Functions

open System
open Day22.Types
open Geometry


let handleMoveRight (state: State) : Point2d =
    let rec loop (p: Point2d) : Point2d =
        if p.X = state.Map.Width then
            { X = 0; Y = p.Y } |> loop
        elif state.Map.Values[p.X, p.Y] = ' ' then
            { X = p.X + 1; Y = p.Y } |> loop
        elif state.Map.Values[p.X, p.Y] = '#' then
            { X = p.X - 1; Y = p.Y }
        elif state.Map.Values[p.X, p.Y] = '.' then
            p
        else
            invalidOp "bad move"

    let newCursor =
        { X = state.Cursor.X + 1
          Y = state.Cursor.Y }
        |> loop

    if state.Map.Values[newCursor.X, newCursor.Y] = ' ' then
        state.Cursor
    else
        newCursor

let handleMoveLeft (state: State) : Point2d =
    let rec loop (p: Point2d) : Point2d =
        if p.X = -1 then
            { X = p.X + state.Map.Width; Y = p.Y } |> loop
        elif state.Map.Values[p.X, p.Y] = ' ' then
            { X = p.X - 1; Y = p.Y } |> loop
        elif state.Map.Values[p.X, p.Y] = '#' then
            { X = p.X + 1; Y = p.Y }
        elif state.Map.Values[p.X, p.Y] = '.' then
            p
        else
            invalidOp "bad move"

    let newCursor =
        { X = state.Cursor.X - 1
          Y = state.Cursor.Y }
        |> loop

    if state.Map.Values[newCursor.X, newCursor.Y] = ' ' then
        state.Cursor
    else
        newCursor

let handleMoveUp (state: State) : Point2d =
    let rec loop (p: Point2d) : Point2d =
        if p.Y = -1 then
            { X = p.X; Y = p.Y + state.Map.Height } |> loop
        elif state.Map.Values[p.X, p.Y] = ' ' then
            { X = p.X; Y = p.Y - 1 } |> loop
        elif state.Map.Values[p.X, p.Y] = '#' then
            { X = p.X; Y = p.Y + 1 }
        elif state.Map.Values[p.X, p.Y] = '.' then
            p
        else
            invalidOp "bad move"

    let newCursor =
        { X = state.Cursor.X
          Y = state.Cursor.Y - 1 }
        |> loop

    if state.Map.Values[newCursor.X, newCursor.Y] = ' ' then
        state.Cursor
    else
        newCursor

let handleMoveDown (state: State) : Point2d =
    let rec loop (p: Point2d) : Point2d =
        if p.Y = state.Map.Height then
            { X = p.X; Y = 0 } |> loop
        elif state.Map.Values[p.X, p.Y] = ' ' then
            { X = p.X; Y = p.Y + 1 } |> loop
        elif state.Map.Values[p.X, p.Y] = '#' then
            { X = p.X; Y = p.Y - 1 }
        elif state.Map.Values[p.X, p.Y] = '.' then
            p
        else
            invalidOp "bad move"

    let newCursor =
        { X = state.Cursor.X
          Y = state.Cursor.Y + 1 }
        |> loop

    if state.Map.Values[newCursor.X, newCursor.Y] = ' ' then
        state.Cursor
    else
        newCursor

let handleRotateLeft (state: State) : Facing =
    match state.Facing with
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

let handleRotateRight (state: State) : Facing =
    match state.Facing with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let handleMove (state: State) : Point2d =
    match state.Facing with
    | Left -> state |> handleMoveLeft
    | Right -> state |> handleMoveRight
    | Up -> state |> handleMoveUp
    | Down -> state |> handleMoveDown

let parseMap (lines: string list) : Map =
    let width = lines[0].Length
    let height = lines.Length - 2
    let values = Array2D.init width height (fun i j -> lines[j][i])

    { Width = width
      Height = height
      Values = values }

let parseOrders (lines: string list) : Order list =
    let line = lines |> List.last

    let splitTurns (s: string) (c: char) : string =
        if c |> Char.IsDigit then
            s + string c
        else
            s + " " + string c + " "

    let updatedLine = line |> Seq.toList |> List.fold splitTurns ""

    let expandOrders (orders: Order list) (token: string) : Order list =
        match token with
        | "L" -> [ RotateLeft ] |> List.append orders
        | "R" -> [ RotateRight ] |> List.append orders
        | _ -> [ for _ in 1 .. (int token) -> Move ] |> List.append orders

    updatedLine.Split [| ' ' |] |> Array.fold expandOrders []

let parseLines (lines: string list) : State =
    { Map = lines |> parseMap
      Orders = lines |> parseOrders
      Facing = Right
      Cursor = { X = lines[ 0 ].IndexOf('.'); Y = 0 } }

let printState (state: State) : unit =
    for j = 0 to state.Map.Height - 1 do
        for i = 0 to state.Map.Width - 1 do
            if i = state.Cursor.X && j = state.Cursor.Y then
                match state.Facing with
                | Up -> printf "^"
                | Right -> printf ">"
                | Left -> printf "<"
                | Down -> printf "v"
            else
                printf "%c" state.Map.Values[i, j]

        printfn ""

    printfn ""

let rec runSimulation (state: State) : State =
    if state.Orders.Length = 0 then
        state
    else
        let order = state.Orders.Head
        let newOrders = state.Orders.Tail

        let newState =
            match order with
            | RotateLeft ->
                { state with
                    Orders = newOrders
                    Facing = state |> handleRotateLeft }
            | RotateRight ->
                { state with
                    Orders = newOrders
                    Facing = state |> handleRotateRight }
            | Move ->
                { state with
                    Orders = newOrders
                    Cursor = state |> handleMove }

        newState |> printState
        newState |> runSimulation

let computeFacingValue (facing: Facing) : int =
    match facing with
    | Right -> 0
    | Down -> 1
    | Left -> 2
    | Up -> 3
