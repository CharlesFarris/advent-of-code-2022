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

type State =
    { Ranges: Range1d list
      Merged: Range1d list
      Y: int }

let computeState (y: int) : State =

    let computeRange (y: int) (sensor: Sensor) : Range1d option =
        let result = Point2d.solveManhattanDistance sensor.Position y sensor.Distance

        // match result with
        // | Some range ->
        //     let d1 = Point2d.manhattanDistance sensor.Position { X = range.Start; Y = y }
        //     let d2 = Point2d.manhattanDistance sensor.Position { X = range.End; Y = y }
        //     printfn "%i %i = %i" d1 d2 sensor.Distance
        // | None -> printfn "None"

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

    let initialState = { Ranges = ranges; Merged = []; Y = y}

    initialState |> mergeRanges
    
let startX = 0
let startY = 0
let endX = 4000000
let endY = 4000000
let rangeX = Range1d.create startX endX

let isGoodState (state: State) : bool =
    not (state.Merged |> List.forall (fun r -> Range1d.containsRange r rangeX))
    
let states =
    [ for y in startY..endY -> computeState y ]
    |> List.filter isGoodState

let containsValue (value: int) (ranges: Range1d list) : bool =
    ranges |> List.exists (fun r -> Range1d.containsValue value r)
    
let getLocations (state: State) (startX: int) (endX: int) : Point2d list =
    if state.Merged.Length = 1 then
        []
    else
        let points = [ for x in startX..endX -> { X = x; Y = state.Y } ]
        let possibles = points |> List.filter (fun p -> not (state.Merged |> containsValue p.X))
        possibles
    
let allPossibles =
    states
    |> List.fold (fun acc elem -> acc |> List.append (getLocations elem startX endX) ) [] 

let beacon =
    if allPossibles.Length = 1 then
        allPossibles.Head
    else
        invalidOp "Invalid number of possibles"

let scalar: int64  = 4000000
let frequency = scalar * (int64 beacon.X)  + (int64 beacon.Y)

printfn "Frequency: %i" frequency

let x = 0    
