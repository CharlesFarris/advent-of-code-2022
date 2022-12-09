namespace Day9.Common

type Move = { Direction: string; Count: int }

type Point = { X: int; Y: int }

module Point =
    let Zero = { X = 0; Y = 0 }

type State =
    { Head: Point
      Tail: Point
      History: Point list }

type Offset = { X: int; Y: int }

module Offset =
    let computeOffset (head: Point) (tail: Point) : Offset =
        { X = head.X - tail.X
          Y = head.Y - tail.Y }

module Move =
    let parseMove (moves: Move list) (line: string) : Move list =
        let tokens = line.Split [| ' ' |]
        let direction = tokens[0]
        let count = int tokens[1]
        List.append moves [ for i in 1..count -> { Direction = direction; Count = 1 } ]

    let handleMove (state: State) (move: Move) : State =
        let head = state.Head

        let newHead =
            match move.Direction with
            | "R" -> { head with X = head.X + 1 }
            | "L" -> { head with X = head.X - 1 }
            | "U" -> { head with Y = head.Y + 1 }
            | "D" -> { head with Y = head.Y - 1 }
            | _ -> head

        let tail = state.Tail
        let offset = Offset.computeOffset newHead tail

        let newTail =
            match offset with
            | { X = 0; Y = 0 } -> tail
            | { X = 1; Y = 0 } -> tail
            | { X = -1; Y = 0 } -> tail
            | { X = 0; Y = 1 } -> tail
            | { X = 0; Y = -1 } -> tail
            | { X = 1; Y = 1 } -> tail
            | { X = -1; Y = 1 } -> tail
            | { X = 1; Y = -1 } -> tail
            | { X = -1; Y = -1 } -> tail

            | { X = 2; Y = 0 } -> { tail with X = tail.X + 1 }
            | { X = -2; Y = 0 } -> { tail with X = tail.X - 1 }
            | { X = 0; Y = 2 } -> { tail with Y = tail.Y + 1 }
            | { X = 0; Y = -2 } -> { tail with Y = tail.Y - 1 }

            | { X = 1; Y = 2 } -> { X = tail.X + 1; Y = tail.Y + 1 }
            | { X = 2; Y = 1 } -> { X = tail.X + 1; Y = tail.Y + 1 }

            | { X = 1; Y = -2 } -> { X = tail.X + 1; Y = tail.Y - 1 }
            | { X = 2; Y = -1 } -> { X = tail.X + 1; Y = tail.Y - 1 }

            | { X = -1; Y = -2 } -> { X = tail.X - 1; Y = tail.Y - 1 }
            | { X = -2; Y = -1 } -> { X = tail.X - 1; Y = tail.Y - 1 }

            | { X = -1; Y = 2 } -> { X = tail.X - 1; Y = tail.Y + 1 }
            | { X = -2; Y = 1 } -> { X = tail.X - 1; Y = tail.Y + 1 }

            | { X = _; Y = _ } -> tail

        { Head = newHead
          Tail = newTail
          History = List.append state.History [ newTail ] }
