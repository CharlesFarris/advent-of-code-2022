
open Map2d

let startValue = -1
let endValue = int 'z' - 96

let mapFun (c: char) : int =
    match c with
    | 'S' -> startValue
    | 'E' -> endValue
    | _ -> int c - 97
    
let map = readMap ".\\test_data.txt" mapFun

printfn "%A" map

let startCell = findCellsByValue map startValue |> List.exactlyOne
let endCell = findCellsByValue map endValue |> List.exactlyOne

printfn "Start: %A" startCell
printfn "End: %A" endCell

let canMove (map: Map2d) (startCell: Cell) (endCell: Cell)  : bool =
    let startValue = getValue map startCell
    let endValue = getValue map endCell
    let delta = endValue - startValue
    delta <= 1
    
let path = Map2d.findPath map startCell endCell canMove

printfn "Length: %i" (path.Cells.Length - 1)
