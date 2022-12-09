namespace Day9.Common

type Move = { Direction: string; Count: int }

type Point = { X: int; Y: int }

module Point =
    let Zero = { X = 0; Y = 0 }

type State =
    { Knots: Point list
      History: Point list }

type Offset = { DX: int; DY: int }

module Offset =
    let computeOffset (head: Point) (tail: Point) : Offset =
        { DX = head.X - tail.X
          DY = head.Y - tail.Y }

module Move =
    let parseMove (moves: Move list) (line: string) : Move list =
        let tokens = line.Split [| ' ' |]
        let direction = tokens[0]
        let count = int tokens[1]
        List.append moves [ for i in 1..count -> { Direction = direction; Count = 1 } ]

    let moveHead (head: Point) (move: Move) : Point =
        match move.Direction with
        | "R" -> { head with X = head.X + 1 }
        | "L" -> { head with X = head.X - 1 }
        | "U" -> { head with Y = head.Y + 1 }
        | "D" -> { head with Y = head.Y - 1 }
        | _ -> head

    let moveTail (head: Point) (tail: Point) : Point =
        let offset = Offset.computeOffset head tail

        match offset with
        | { DX = 0; DY = 0 } -> tail
        | { DX = 1; DY = 0 } -> tail
        | { DX = -1; DY = 0 } -> tail
        | { DX = 0; DY = 1 } -> tail
        | { DX = 0; DY = -1 } -> tail
        | { DX = 1; DY = 1 } -> tail
        | { DX = -1; DY = 1 } -> tail
        | { DX = 1; DY = -1 } -> tail
        | { DX = -1; DY = -1 } -> tail

        | { DX = 2; DY = 0 } -> { tail with X = tail.X + 1 }
        | { DX = -2; DY = 0 } -> { tail with X = tail.X - 1 }
        | { DX = 0; DY = 2 } -> { tail with Y = tail.Y + 1 }
        | { DX = 0; DY = -2 } -> { tail with Y = tail.Y - 1 }

        | { DX = 1; DY = 2 } -> { X = tail.X + 1; Y = tail.Y + 1 }
        | { DX = 2; DY = 2 } -> { X = tail.X + 1; Y = tail.Y + 1 }
        | { DX = 2; DY = 1 } -> { X = tail.X + 1; Y = tail.Y + 1 }

        | { DX = 1; DY = -2 } -> { X = tail.X + 1; Y = tail.Y - 1 }
        | { DX = 2; DY = -2 } -> { X = tail.X + 1; Y = tail.Y - 1 }
        | { DX = 2; DY = -1 } -> { X = tail.X + 1; Y = tail.Y - 1 }

        | { DX = -1; DY = -2 } -> { X = tail.X - 1; Y = tail.Y - 1 }
        | { DX = -2; DY = -2 } -> { X = tail.X - 1; Y = tail.Y - 1 }
        | { DX = -2; DY = -1 } -> { X = tail.X - 1; Y = tail.Y - 1 }

        | { DX = -1; DY = 2 } -> { X = tail.X - 1; Y = tail.Y + 1 }
        | { DX = -2; DY = 2 } -> { X = tail.X - 1; Y = tail.Y + 1 }
        | { DX = -2; DY = 1 } -> { X = tail.X - 1; Y = tail.Y + 1 }

        | { DX = _; DY = _ } -> tail

    let moveKnots (knots: Point list) (move: Move) : Point list =
        knots
        |> List.fold
            (fun acc elem ->
                if acc.Length = 0 then
                    List.append acc [ moveHead elem move ]
                else
                    List.append acc [ moveTail (List.last acc) elem ])
            []

    let handleMove (state: State) (move: Move) : State =
        let knots = moveKnots state.Knots move

        { Knots = knots
          History = List.append state.History [ knots |> List.last ] }
