open System.Diagnostics
open System.IO

type Cube = { X: int; Y: int; Z: int }

type FaceId =
    | XN
    | XP
    | YN
    | YP
    | ZN
    | ZP

type Face = { Cube: Cube; Id: FaceId }

let createCube (x: int) (y: int) (z: int) : Cube = { X = x; Y = y; Z = z }

let parseCube (line: string) : Cube =
    let tokens = line.Split [| ',' |]

    { X = int tokens[0]
      Y = int tokens[1]
      Z = int tokens[2] }

let getAdjacents (cube: Cube) : Cube list =
    [ createCube (cube.X - 1) cube.Y cube.Z // XN
      createCube (cube.X + 1) cube.Y cube.Z // XP
      createCube cube.X (cube.Y - 1) cube.Z // YN
      createCube cube.X (cube.Y + 1) cube.Z // YP
      createCube cube.X cube.Y (cube.Z - 1) // ZN
      createCube cube.X cube.Y (cube.Z + 1) ] // ZP

let cubes = ".\\test_data.txt" |> File.ReadLines |> Seq.toList |> List.map parseCube

let generateFaces (faces: Face list) (cube: Cube) : Face list =
    [ { Id = XN; Cube = cube }
      { Id = YN; Cube = cube }
      { Id = ZN; Cube = cube }
      { Id = XP; Cube = cube }
      { Id = YP; Cube = cube }
      { Id = ZP; Cube = cube } ]
    |> List.append faces

let faces = cubes |> List.fold generateFaces []

let findCoveredFaces (id: FaceId) (cubes: Cube list) (cube: Cube) : Face list =
    match id with
    | XP ->
        let adjacent =
            { X = cube.X + 1
              Y = cube.Y
              Z = cube.Z }

        match cubes |> List.tryFind (fun c -> c = adjacent) with
        | Some _ -> [ { Id = XP; Cube = cube }; { Id = XN; Cube = adjacent } ]
        | None -> []
    | XN ->
        let adjacent =
            { X = cube.X - 1
              Y = cube.Y
              Z = cube.Z }

        match cubes |> List.tryFind (fun c -> c = adjacent) with
        | Some _ -> [ { Id = XN; Cube = cube }; { Id = XP; Cube = adjacent } ]
        | None -> []
    | YP ->
        let adjacent =
            { X = cube.X
              Y = cube.Y + 1
              Z = cube.Z }

        match cubes |> List.tryFind (fun c -> c = adjacent) with
        | Some _ -> [ { Id = YP; Cube = cube }; { Id = YN; Cube = adjacent } ]
        | None -> []
    | YN ->
        let adjacent =
            { X = cube.X 
              Y = cube.Y - 1
              Z = cube.Z }

        match cubes |> List.tryFind (fun c -> c = adjacent) with
        | Some _ -> [ { Id = YN; Cube = cube }; { Id = YP; Cube = adjacent } ]
        | None -> []
    | ZP ->
        let adjacent =
            { X = cube.X 
              Y = cube.Y
              Z = cube.Z + 1}

        match cubes |> List.tryFind (fun c -> c = adjacent) with
        | Some _ -> [ { Id = ZP; Cube = cube }; { Id = ZN; Cube = adjacent } ]
        | None -> []
    | ZN ->
        let adjacent =
            { X = cube.X
              Y = cube.Y
              Z = cube.Z - 1 }

        match cubes |> List.tryFind (fun c -> c = adjacent) with
        | Some _ -> [ { Id = ZN; Cube = cube }; { Id = ZP; Cube = adjacent } ]
        | None -> []

let findAllCoveredFaces (cubes: Cube list) (cube: Cube) : Face list =
    cube |> (findCoveredFaces XN cubes)
    |> List.append  (cube |> (findCoveredFaces XP cubes))
    |> List.append  (cube |> (findCoveredFaces YN cubes))
    |> List.append  (cube |> (findCoveredFaces YP cubes))
    |> List.append  (cube |> (findCoveredFaces ZN cubes))
    |> List.append  (cube |> (findCoveredFaces ZP cubes))
    
let allCoveredFaces =
    cubes
    |> List.fold (fun faces cube -> (cube |> findAllCoveredFaces cubes) |> List.append faces) []
    |> List.distinct
    
let uncoveredFaces = faces |> List.filter (fun f -> not (List.contains f allCoveredFaces))

printfn "Uncovered Faces: %i" uncoveredFaces.Length