open System.IO
open Range

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

let cubes = ".\\part1_data.txt" |> File.ReadLines |> Seq.toList |> List.map parseCube

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
              Z = cube.Z + 1 }

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
    cube
    |> (findCoveredFaces XN cubes)
    |> List.append (cube |> (findCoveredFaces XP cubes))
    |> List.append (cube |> (findCoveredFaces YN cubes))
    |> List.append (cube |> (findCoveredFaces YP cubes))
    |> List.append (cube |> (findCoveredFaces ZN cubes))
    |> List.append (cube |> (findCoveredFaces ZP cubes))

let allCoveredFaces =
    cubes
    |> List.fold (fun faces cube -> (cube |> findAllCoveredFaces cubes) |> List.append faces) []
    |> List.distinct

let uncoveredFaces =
    faces |> List.filter (fun f -> not (List.contains f allCoveredFaces))

printfn "Uncovered Faces: %i" uncoveredFaces.Length

type BoundingBox = { X: Range1d; Y: Range1d; Z: Range1d }

let xValues = cubes |> List.map (fun c -> c.X) |> List.distinct
let minX = xValues |> List.min
let maxX = xValues |> List.max

let yValues = cubes |> List.map (fun c -> c.Y) |> List.distinct
let minY = yValues |> List.min
let maxY = yValues |> List.max

let zValues = cubes |> List.map (fun c -> c.Z) |> List.distinct
let minZ = zValues |> List.min
let maxZ = zValues |> List.max

let expandRange (r1: Range1d) : Range1d =
    let or1 = Range1d.order r1
    Range1d.create (or1.Start - 1) (or1.End + 1)

let box =
    { X = Range1d.create minX maxX |> expandRange
      Y = Range1d.create minY maxY |> expandRange
      Z = Range1d.create minZ maxZ |> expandRange }

let isInside (box: BoundingBox) (cube: Cube) : bool =
    box.X |> Range1d.containsValue cube.X
    && box.Y |> Range1d.containsValue cube.Y
    && box.Z |> Range1d.containsValue cube.Z

let getAdjacent (cube: Cube) : Cube list =
    [ { X = cube.X - 1
        Y = cube.Y
        Z = cube.Z }
      { X = cube.X + 1
        Y = cube.Y
        Z = cube.Z }
      { X = cube.X
        Y = cube.Y - 1
        Z = cube.Z }
      { X = cube.X
        Y = cube.Y + 1
        Z = cube.Z }
      { X = cube.X
        Y = cube.Y
        Z = cube.Z + 1 }
      { X = cube.X
        Y = cube.Y
        Z = cube.Z - 1 } ]

let checkAdjacent (box: BoundingBox) (stack: Cube list) (cubes1: Cube list) (cubes2: Cube list) (cube: Cube) : bool =
    if not (cube |> isInside box) then false
    elif stack |> List.contains cube then false
    elif cubes1 |> List.contains cube then false
    elif cubes2 |> List.contains cube then false
    else true

let mutable exteriorCubes: Cube list = []

let mutable stack = [ createCube box.X.Start box.Y.Start box.Z.Start ]

while stack.Length > 0 do
    let current = stack.Head
    stack <- stack.Tail

    if
        not (cubes |> List.contains current)
        && not (exteriorCubes |> List.contains current)
    then
        exteriorCubes <- [ current ] |> List.append exteriorCubes

    let adjacent =
        current
        |> getAdjacent
        |> List.filter (checkAdjacent box stack cubes exteriorCubes)

    stack <- adjacent |> List.append stack

let exc = exteriorCubes |> List.append []
printfn "%i %i" exteriorCubes.Length exc.Length

let checkFace (cubes: Cube list) (face: Face) : bool =
    let adjacent =
        match face.Id with
        | XN -> createCube (face.Cube.X - 1) face.Cube.Y face.Cube.Z
        | XP -> createCube (face.Cube.X + 1) face.Cube.Y face.Cube.Z
        | YN -> createCube face.Cube.X (face.Cube.Y - 1) face.Cube.Z
        | YP -> createCube face.Cube.X (face.Cube.Y + 1) face.Cube.Z
        | ZN -> createCube face.Cube.X face.Cube.Y (face.Cube.Z - 1)
        | ZP -> createCube face.Cube.X face.Cube.Y (face.Cube.Z + 1)

    cubes |> List.contains adjacent

let exteriorFaces = uncoveredFaces |> List.filter (fun f -> f |> checkFace exc)

printfn "Exterior Faces: %i" exteriorFaces.Length
