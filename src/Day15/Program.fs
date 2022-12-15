open System.IO
open Geometry
open Range

type Sensor =
    { Position: Point2d
      Beacon: Point2d
      Distance: int }

let parseLine (line: string) : Sensor =
    // Sensor at x=2, y=18: closest beacon is at x=-2, y=15
    let tokens =
        line
            .Replace("Sensor at x=", "")
            .Replace(" y=", "")
            .Replace(": closest beacon is at x=", ",")
            .Split [| ',' |]

    let position = { X = int tokens[0]; Y = int tokens[1] }
    let beacon = { X = int tokens[2]; Y = int tokens[3] }

    { Position = position
      Beacon = beacon
      Distance = Point2d.manhattanDistance position beacon }

let sensorData =
    File.ReadLines ".\\part1_data.txt" |> Seq.toList |> List.map parseLine

let y = 2000000

let computeRange (y: int) (sensor: Sensor) : Range1d option =
    let result = Point2d.solveManhattanDistance sensor.Position y sensor.Distance

    match result with
    | Some range ->
        let d1 = Point2d.manhattanDistance sensor.Position { X = range.Start; Y = y }
        let d2 = Point2d.manhattanDistance sensor.Position { X = range.End; Y = y }
        printfn "%i %i = %i" d1 d2 sensor.Distance
    | None -> printfn "None"

    result

let ranges =
    sensorData
    |> List.map (computeRange y)
    |> List.filter (fun ro ->
        match ro with
        | Some _ -> true
        | None -> false)
    |> List.map (fun ro ->
        let empty = Range1d.create 0 0

        match ro with
        | Some r -> r
        | None -> empty)

type State =
    { Ranges: Range1d list
      Merged: Range1d list }

let unionRange (ranges: Range1d list) : Range1d =
    let startValue = ranges |> List.map (fun r -> min r.Start r.End) |> List.min
    let endValue = ranges |> List.map (fun r -> max r.Start r.End) |> List.max
    Range1d.create startValue endValue

let findIntersecting (range: Range1d) (ranges: Range1d list) : Range1d list =
    ranges |> List.filter (Range1d.isIntersecting range)

let rec mergeRanges (state: State) : State =
    if state.Ranges.Length = 0 then
        state
    else
        let head = state.Ranges.Head
        let tail = state.Ranges.Tail

        let newMerged =
            if state.Merged.Length = 0 then
                [ head ]
            else
                let intersecting = findIntersecting head state.Merged

                if intersecting.Length = 0 then
                    [ head ] |> List.append state.Merged
                else
                    let union = unionRange ([ head ] |> List.append intersecting)

                    let nonIntersecting =
                        state.Merged |> List.filter (fun r -> not (intersecting |> List.contains r))

                    [ union ] |> List.append nonIntersecting

        let newState =
            { state with
                Ranges = tail
                Merged = newMerged }

        mergeRanges newState

let initialState = { Ranges = ranges; Merged = [] }

let finalState = initialState |> mergeRanges

let beaconPositions =
    sensorData
    |> List.filter (fun s -> s.Beacon.Y = y)
    |> List.map (fun s -> s.Beacon.X)
    |> List.distinct
    
let overlappingBeacons =
    beaconPositions
    |> List.filter (fun x -> finalState.Merged |> List.exists (Range1d.containsValue x))

let sum = finalState.Merged |> List.map Range1d.size |> List.sum

printfn "sum: %A" (sum - overlappingBeacons.Length)
