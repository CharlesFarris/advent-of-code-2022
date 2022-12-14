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
    ".\\test_data.txt"
    |> File.ReadAllLines
    |> Seq.toList
    |> List.fold parsePaths []
    
let findMaxRow (path: Cell list) : int =
    path |> List.fold (fun acc cell -> max acc cell.Row) 0
    
let findMaxColum (path: Cell list) : int =
    path |> List.fold (fun acc cell -> max acc cell.Column) 0
    
let maxRow = paths |> List.fold (fun acc path -> max acc (findMaxRow path)) 0
let maxColumn = paths |> List.fold (fun acc path -> max acc (findMaxColum path)) 0

let emptyMap =  Map2d.createMap (maxRow + 1) (maxColumn + 1)

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

let addRocksToMap (map: Map2d) (cells: Cell list) : Map2d =
    cells |> List.fold (fun acc elem -> setValue acc elem 1) map

let startingMap = addRocksToMap emptyMap rocks
    
type State = {
    Units : int
    Map: Map2d
    IsRunning : bool
    Cell: Cell
    StartCell: Cell
}

let startCell = {Row = 500; Column = 0}

let initialState = {
    StartCell = startCell 
    Cell = startCell
    Units = 0
    Map = startingMap
    IsRunning = true
}

let updateState (state: State) : State =
    let moveCell (dr: int) (dc: int) (cell: Cell) : Cell =
        { Row = cell.Row + dr; Column = cell.Column + dc }
            
    let down = state.Cell |> moveCell 0 1

    if Map2d.inMap state.Map down then
        if Map2d.getValue state.Map down = 0 then
            {state with Cell = down }
        else
            let left = state.Cell |> moveCell -1 1
            if Map2d.inMap state.Map left then
                if getValue state.Map left = 0 then
                    {state with Cell = left }
                else
                    let right = state.Cell |> moveCell 1 1
                    if inMap state.Map right then
                        if getValue state.Map right = 0 then
                            { state with Cell = right }
                        else
                            { state with Map = setValue state.Map state.Cell 1; Cell = startCell; Units = state.Units + 1}
                    else
                        {state with IsRunning = false}
            else
                {state with IsRunning = false }
    else
        {state with IsRunning = false}
    
let rec runSimulation (updateFun : State -> State) (state: State) : State =
    match state.IsRunning with
    | false -> state
    | true -> (updateFun state) |> runSimulation updateFun          

let finalState = runSimulation updateState initialState

printfn "Units: %i" finalState.Units
    
   