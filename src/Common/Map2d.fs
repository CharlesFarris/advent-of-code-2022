module Map2d

open System.IO
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Cell = { Row: int; Column: int }

type Map2d = {
    Rows: int
    Columns: int
    Values: int[,]
}

let createMap (rows: int) (columns: int) : Map2d =
    {
        Rows = rows
        Columns = columns
        Values = Array2D.init rows columns (fun i j -> 0) 
    }
    
let readMap (path : string) (mapFun: char -> int) : Map2d =
    let input =
        path
        |> File.ReadAllLines
        |> Seq.toList

    let rows = input.Length
    let columns = input[0].Length

    {
        Rows = rows
        Columns = columns
        Values = Array2D.init rows columns (fun i j -> input[i][j] |> mapFun) 
    }

let getAllCells (map: Map2d) : Cell list =
    [ for r in 0 .. (map.Rows - 1) -> r ]
    |> List.collect (fun r -> [ for c in 0 .. (map.Columns - 1) -> { Row = r; Column = c } ])

let findCellsByValue (map: Map2d) (value: int) : Cell list =
    let cells = map |> getAllCells
    let findCell (localMap: Map2d) (value: int) (cells: Cell list) (cell: Cell) : Cell list =
        if localMap.Values[cell.Row, cell.Column] = value then
            [ cell ] |> List.append cells
        else
            cells
    cells |> List.fold (findCell map value) []
    
let inMap (map: Map2d) (cell: Cell) : bool =
    0 <= cell.Row && cell.Row < map.Rows && 0 <= cell.Column && cell.Column < map.Columns

let getAdjacentCells (map: Map2d) (cell: Cell) : Cell list =
    [ { cell with Row = cell.Row - 1 }
      { cell with Row = cell.Row + 1 }
      { cell with Column = cell.Column - 1 }
      { cell with Column = cell.Column + 1 }]
    |> List.filter (fun c -> c |> inMap map)
    
let getValue (map: Map2d) (cell: Cell) =
    if inMap map cell then
        map.Values[cell.Row, cell.Column]
    else
        invalidOp "Invalid cell"
        
let setValue (map: Map2d) (cell: Cell) (value: int) =
    let newValues = map.Values |> Array2D.mapi (fun r c v -> if cell.Row = r && cell.Column = c then value else v)
    {map with Values = newValues}

type Path = {
    Cells : Cell list
}

let printCells (label: string) (cells: Cell list) =
    printf "%s : " label
    cells |> List.rev |> List.iter (fun c -> printf "%i,%i " c.Row c.Column)
    printfn ""

let printPath (path: Path) =
    path.Cells |> List.rev |> List.iter (fun c -> printf "%i,%i " c.Row c.Column)
    printfn " %i" path.Cells.Length
   
let printPaths (paths: Path list) =
    printfn "Paths: %i" paths.Length
    paths |> List.iter printPath
    printfn ""

let findPath (map: Map2d) (startCell: Cell) (endCell: Cell) (canMove: Map2d -> Cell -> Cell -> bool) : Path option =
    
    let notVisited (path: Path) (cell: Cell) : bool =
        not (path.Cells |> List.contains cell)
        
    let mutable paths = [ { Cells = [ startCell ] } ]
    let mutable foundPaths : Path list = []
    let mutable visited : Cell list = [ startCell ]
    while paths.Length > 0 do
        //paths |> printPaths
        //visited |> printCells "visited"
        let current = paths.Head
        //current |> printPath
        paths <- paths.Tail
        let last = current.Cells.Head
        let adjacentCells = getAdjacentCells map last
        //adjacentCells |> printCells "adjacent"
        let notVisitedCells = adjacentCells |> List.filter (fun c -> not (visited |> List.contains c))
        //notVisitedCells |> printCells "not visited"
        let canMoveCells = notVisitedCells |> List.filter (canMove map last)
        //canMoveCells |> printCells "can move"
        visited <- visited |> List.append canMoveCells
        let newPaths =
            canMoveCells |> List.map (fun c -> { current with Cells = current.Cells |> List.append [ c ] })
        let completePaths = newPaths |> List.filter (fun p -> p.Cells.Head = endCell)
        let incompletePaths = newPaths |> List.filter (fun p -> p.Cells.Head <> endCell)
        foundPaths <- foundPaths |> List.append completePaths
        paths <-
            match incompletePaths.Length with
            | 0 -> paths
            | _ -> incompletePaths |> List.append paths

    foundPaths |> printPaths
    if foundPaths.IsEmpty then
        None
    else
        Some (foundPaths
            |> List.sortBy (fun p -> p.Cells.Length)
            |> List.head)

let parseCell (line: string) : Cell =
    let tokens = line.Split [| ',' |]
    { Row = int tokens[0]; Column = int tokens[1] }