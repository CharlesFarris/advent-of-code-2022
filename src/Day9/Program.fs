open System.IO

type Move = { Direction: string; Count: int }

let parseMove (moves: Move list) (line: string) : Move list =
    let tokens = line.Split [| ' ' |]
    let direction = tokens[0]
    let count = int tokens[1]
    List.append moves [ for i in 1..count -> { Direction = direction; Count = 1 } ]

let moves =
    ".\\test_data.txt" |> File.ReadAllLines |> Seq.toList |> List.fold parseMove []

type Point = { X: int; Y: int }

type State =
    { Head: Point
      Tail: Point
      History: Point list }

type Offset = { X: int; Y: int }

let computeOffset (head: Point) (tail: Point) : Offset =
    { X = head.X - tail.X
      Y = head.Y - tail.Y }

let initialState =
    { Head = { X = 0; Y = 0 }
      Tail = { X = 0; Y = 0 }
      History = [] }

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
    let offset = computeOffset newHead tail

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

    printfn "Head %i,%i" newHead.X newHead.Y
    printfn "Tail %i,%i" newTail.X newTail.Y

    { Head = newHead
      Tail = newTail
      History = List.append state.History [ state.Tail ] }

let finalState = moves |> List.fold handleMove initialState

let distinctHistory = finalState.History |> List.distinct

printfn "Count: %i" distinctHistory.Length
