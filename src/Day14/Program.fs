open System.IO
open Map2d
open Microsoft.FSharp.Core

let parsePath (line: string) : Cell list =
    let tokens = line.Split " -> " |> Seq.toList
    tokens
    |> List.fold (fun cells l -> [ l |> parseCell ] |> List.append cells ) []
    
let parsePaths (paths: Cell list list) (line: string) : Cell list list =
    [ line |> parsePath ] |> List.append paths
    
let paths =
    ".\\part1_data.txt"
    |> File.ReadAllLines
    |> Seq.toList
    |> List.fold parsePaths []
    
let findMaxRow (path: Cell list) : int =
    path |> List.fold (fun acc cell -> max acc cell.Row) 0
    
let findMaxColum (path: Cell list) : int =
    path |> List.fold (fun acc cell -> max acc cell.Column) 0
    
let maxRow = paths |> List.fold (fun acc path -> max acc (findMaxRow path)) 0
let maxColumn = paths |> List.fold (fun acc path -> max acc (findMaxColum path)) 0

let buildPath (path: Cell list) : Cell list =
    let buildPathSegment (s: Cell) (e: Cell) : Cell list = 
        if s.Row = e.Row then
            if s.Column < e.Column then
                [ for c in s.Column..e.Column -> { Row = s.Row; Column = c } ]
            else
                [ for c in e.Column..s.Column -> { Row = s.Row; Column = c } ]
        else
            if s.Row < e.Row then
                [ for r in s.Row..e.Row -> { Row = r; Column = s.Column }]
            else
                [ for r in e.Row..s.Row -> { Row = r; Column = s.Column }]
    let segments = [for i in 0..(path.Length - 2) -> buildPathSegment path[i] path[i + 1]]
    segments |> List.fold (fun acc s -> s |> List.append acc) []

let rocks = paths |> List.fold (fun acc path -> acc |> List.append (buildPath path)) []
    
type State = {
    Units : int
    Rocks: Cell list
    IsRunning : bool
    Cell: Cell
    StartCell: Cell
    Floor: int
}

let startCell = {Row = 500; Column = 0}

let initialState = {
    StartCell = startCell 
    Cell = startCell
    Units = 0
    Rocks = rocks
    IsRunning = true
    Floor = maxColumn + 2
}

let updateState (state: State) : State =
    let moveCell (dr: int) (dc: int) (cell: Cell) : Cell =
        { Row = cell.Row + dr; Column = cell.Column + dc }

    if state.Cell.Column = state.Floor - 1 then
        printfn "%i" state.Units
        { state with Rocks = [ state.Cell ] |> List.append state.Rocks; Cell = startCell; Units = state.Units + 1 }
    else
        let down = state.Cell |> moveCell 0 1
        if state.Rocks |> List.contains down then
            let left = state.Cell |> moveCell -1 1
            if state.Rocks |> List.contains left then
                let right = state.Cell |> moveCell 1 1
                if state.Rocks |> List.contains right then
                    if state.Cell = startCell then
                        { state with Units = state.Units + 1; IsRunning = false }
                    else
                        printfn "%i" state.Units
                        { state with Rocks = [ state.Cell ] |> List.append state.Rocks; Cell = startCell; Units = state.Units + 1 }
                else
                    { state with Cell = right }
            else
                { state with Cell = left }
        else
            { state with Cell = down }
    
let rec runSimulation (updateFun : State -> State) (state: State) : State =
    match state.IsRunning with
    | false -> state
    | true -> (updateFun state) |> runSimulation updateFun          

let finalState = runSimulation updateState initialState

printfn "Units: %i" finalState.Units
    
   