open System.IO

let input =
    ".\\part1_data.txt"
    |> File.ReadAllLines
    |> Seq.toList

let rows = input.Length
let columns = input[0].Length

let map =
    Array2D.init rows columns (fun i j -> int (input[i][j]) - 48)

printfn "%A" map

let isEdge (row: int) (column: int) (rows: int) (columns: int) : bool =
    row = 0
    || row = rows - 1
    || column = 0
    || column = columns - 1

let computeScenicScore (tuple: int * int) (rows: int) (columns: int) (map: int [,]) : int =
    let row = fst tuple
    let column = snd tuple

    if (isEdge row column rows columns) then
        0
    else
        let height = map[row, column]

        let northSlice =
            [ for r in (row - 1) .. -1 .. 0 -> map[r, column] ]

        let northIndex =
            northSlice
            |> List.tryFindIndex (fun h -> h >= height)

        let north =
            match northIndex with
            | None -> northSlice.Length
            | Some index -> index + 1

        let southSlice =
            [ for r in (row + 1) .. (rows - 1) -> map[r, column] ]

        let southIndex =
            southSlice
            |> List.tryFindIndex (fun h -> h >= height)

        let south =
            match southIndex with
            | None -> southSlice.Length
            | Some index -> index + 1

        let westSlice =
            [ for c in (column - 1) .. -1 .. 0 -> map[row, c] ]

        let westIndex =
            westSlice
            |> List.tryFindIndex (fun h -> h >= height)

        let west =
            match westIndex with
            | None -> westSlice.Length
            | Some index -> index + 1

        let eastSlice =
            [ for c in (column + 1) .. (columns - 1) -> map[row, c] ]

        let eastIndex =
            eastSlice
            |> List.tryFindIndex (fun h -> h >= height)

        let east =
            match eastIndex with
            | None -> eastSlice.Length
            | Some index -> index + 1

        let scenicScore = north * south * west * east
    
        printfn "%i,%i %i,%i -> %i" row column rows columns scenicScore
        
        scenicScore

let coords =
    [ for r in 0 .. (rows - 1) -> r ]
    |> List.collect (fun r -> [ for c in 0 .. (columns - 1) -> (r, c) ])

printfn "%A" coords

let maxScenicScores = coords |> List.map (fun t -> computeScenicScore t rows columns map) |> List.max

printfn "%i" maxScenicScores
