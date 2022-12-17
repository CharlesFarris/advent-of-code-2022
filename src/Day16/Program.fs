open System.IO

type Room =
    { Name: string
      FlowRate: int
      TunnelsTo: string list }

// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
let parseRoom (line: string) : Room =
    let tokens =
        line
            .Replace("Valve ", "")
            .Replace(" has flow rate=", ";")
            .Replace(" tunnels lead to valves ", "")
            .Replace(" tunnel leads to valve ", "")
            .Split([| ';' |])

    let tunnels =
        tokens[ 2 ].Split [| ',' |] |> Array.toList |> List.map (fun t -> t.Trim())

    { Name = tokens[0]
      FlowRate = int tokens[1]
      TunnelsTo = tunnels }

let rooms =
    ".\\test_data.txt" |> File.ReadAllLines |> Seq.toList |> List.map parseRoom

type Edge = { StartIndex: int; EndIndex: int }

let getEdges (rooms: Room list) (edges: Edge list) (room: Room) : Edge list =
    let startIndex = rooms |> List.findIndex (fun r -> r.Name = room.Name)

    let newEdges =
        [ for t in room.TunnelsTo ->
              let endIndex = rooms |> List.findIndex (fun r -> r.Name = t)

              { StartIndex = startIndex
                EndIndex = endIndex } ]

    newEdges |> List.append edges

let edges = rooms |> List.fold (getEdges rooms) []

let maxValue = 4096

type PathData = { Dist: int[,]; Next: int[,] }

let FloydWarshallWithPathReconstruction (edges: Edge list) : PathData =

    let mapDist (array: int[,]) (edge: Edge) : int[,] =
        array[edge.StartIndex, edge.EndIndex] <- 1
        array

    let mapNext (array: int[,]) (edge: Edge) : int[,] =
        array[edge.StartIndex, edge.EndIndex] <- edge.EndIndex
        array

    let mutable dist =
        edges
        |> List.fold mapDist (Array2D.init rooms.Length rooms.Length (fun i j -> if i = j then 0 else maxValue))

    let mutable next =
        edges
        |> List.fold mapNext (Array2D.init rooms.Length rooms.Length (fun i j -> if i = j then j else -1))

    for i = 0 to rooms.Length - 1 do
        for j = 0 to rooms.Length - 1 do
            for k = 0 to rooms.Length - 1 do
                if dist[i, j] > dist[i, k] + dist[k, j] then
                    dist[i, j] <- dist[i, k] + dist[k, j]
                    next[i, j] <- next[i, k]

    { Dist = dist; Next = next }

let data = edges |> FloydWarshallWithPathReconstruction

let getPath (u: int) (v: int) (pathData: PathData) : int list =
    if pathData.Next[u, v] = -1 then
        []
    else
        let mutable path = [u]
        let mutable c = u
        while c <> v do
            c <- pathData.Next[c, v]
            path <- [c] |> List.append path
        path

let paths = [ for i in 1..(rooms.Length - 1) -> data |> getPath 0 i ]

printfn "%A" paths

// printfn "%A" rooms
//
// type State =
//     { Time: int
//       Pressure: int
//       Name: string
//       Path: string
//       Rooms: Room list }
//
// let getPressure (state: State) : int =
//     let sumPressure (sum: int) (room: Room) : int =
//         match room.IsClosed with
//         | false -> sum + room.FlowRate
//         | true -> sum
//
//     state.Rooms |> List.fold sumPressure 0
//
// let initialState =
//     { Time = 0
//       Name = "AA"
//       Path = "AA"
//       Rooms = rooms
//       Pressure = 0 }
//
// let openValve (state: State) : State =
//     let newTime = state.Time + 1
//     let newPressure = state.Pressure + getPressure state
//
//     let newRooms =
//         [ for r in state.Rooms ->
//               if r.Name = state.Name then
//                   { r with IsClosed = false }
//               else
//                   r ]
//
//     { state with
//         Time = newTime
//         Rooms = newRooms
//         Pressure = newPressure }
//
// let moveRoom (name: string) (state: State) : State =
//     let newTime = state.Time + 1
//     let newPressure = state.Pressure + getPressure state
//     let newPath = name + state.Path
//
//     { state with
//         Name = name
//         Path = newPath
//         Time = newTime
//         Pressure = newPressure }
//
// let getRoom (name: string) (state: State) : Room =
//     state.Rooms |> List.find (fun r -> r.Name = name)
//
// let mutable states: State list =
//     [ { Time = 0
//         Name = "AA"
//         Path = "AA"
//         Pressure = 0
//         Rooms = rooms } ]
//
// let mutable finalStates: State list = []
//
// while states.Length > 0 do
//     let state = states.Head
//     let oldStates = states.Tail
//
//     if state.Time = 30 then
//         finalStates <- [ state ] |> List.append finalStates
//         states <- oldStates
//     else
//         let room = state |> getRoom state.Name
//
//         if room.IsClosed then
//             states <- [ state |> openValve ] |> List.append oldStates
//         else
//             let newStates =
//                 [ for name in room.Tunnels ->
//                       let target = state |> getRoom name
//
//                       if target.IsClosed then
//                           Some(state |> moveRoom name)
//                       elif target.IsLeaf then
//                           None
//                       else
//                           let previous = state.Path[2..3]
//
//                           if target.Name = previous then
//                               None
//                           else
//                               Some(state |> moveRoom name) ]
//                 |> List.choose id
//
//             states <- newStates |> List.append oldStates
//
// printfn "%A" finalStates
//
// //printfn "Sum: %i" finalState.Pressure
