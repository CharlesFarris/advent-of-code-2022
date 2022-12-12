
open Map2d

let startValue = -1
let endValue = int 'z' - 96

let mapFun (c: char) : int =
    match c with
    | 'S' -> startValue
    | 'E' -> endValue
    | _ -> int c - 97
    
let map = readMap ".\\part1_data.txt" mapFun

printfn "%A" map

let startCells = findCellsByValue map startValue |> List.append (findCellsByValue map 0)
let endCell = findCellsByValue map endValue |> List.exactlyOne

printfn "Start: %A" startCells
printfn "End: %A" endCell

let canMove (map: Map2d) (startCell: Cell) (endCell: Cell)  : bool =
    let startValue = getValue map startCell
    let endValue = getValue map endCell
    let delta = endValue - startValue
    delta <= 1
    
let paths =
    startCells
    |> List.map (fun c -> findPath map c endCell canMove)
    |> List.map (fun o ->
        match o with
        | Some p -> p.Cells.Length - 1
        | None -> 2147483647 )

printfn "Length: %i" (paths |> List.min)
