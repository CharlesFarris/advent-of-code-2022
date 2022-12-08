open System.IO

let input =
    ".\\part1_data.txt"
    |> File.ReadAllLines
    |> Seq.toList

let rows = input.Length
let columns = input[0].Length

let map = Array2D.init rows columns (fun i j -> int( input[i][j])-48)

printfn "%A" map 

let isEdge (row: int) (column: int) (rows: int) (columns: int) : bool =
    row = 0 || row = rows - 1 || column = 0 || column = columns - 1
    
let countVisible (tuple: int*int) (rows: int) (columns: int) (map : int[, ])  : int =
    let row = fst tuple
    let column = snd tuple
    printfn "%i,%i %i,%i" row column rows columns
    if (isEdge row column rows columns) then
        1
    else
        let north = [for r in 0..(row-1) -> map[r, column]] |> List.max
        let south = [for r in (row+1)..(rows-1) -> map[r, column]] |> List.max
        let west = [for c in 0..(column-1) -> map[row, c]] |> List.max
        let east = [for c in (column+1)..(columns-1) -> map[row, c]] |> List.max
        let height = map[row, column]
        if north < height || south < height || west < height || east < height then
            1
        else
            0

let coords = [for r in 0..(rows-1) ->r ] |> List.collect (fun r -> [for c in 0..(columns-1) -> (r, c)])

printfn "%A" coords
let count = List.fold (fun c t -> c + countVisible t rows columns map) 0 coords
        
printfn "%i" count