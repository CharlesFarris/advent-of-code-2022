open System.IO
open Geometry
open Microsoft.FSharp.Core

let jets =
    File.ReadAllText ".\\part1_data.txt"
    |> Seq.toList
    |> List.map (fun c -> if c = '>' then 1 else -1)

type Chamber = {
    Height: int
    Blocks: int[,]
}

let expandBlocks (height: int) (chamber: Chamber) : int[,] =
    let oldLength2 = chamber.Blocks |> Array2D.length2
    let newLength2 = height + 9
    if newLength2 > oldLength2 then
        let newBlocks = Array2D.zeroCreate 7 newLength2
        newBlocks[0..6, 0..(oldLength2 - 1)] <- chamber.Blocks
        newBlocks
    else
        chamber.Blocks

type Rock =
    { Index: int
      Points: Point2d list
      Left: int
      Right: int
      Top: int
      Bottom: int }
    
let EmptyRock = { Index = -1; Points = []; Left = -1; Right = -1; Bottom = -1; Top = -1}

let getRock (index: int) : Rock =
    match index with
    | 0 ->
        { Index = 0
          Points =
            [ Point2d.create 0 0
              Point2d.create 1 0
              Point2d.create 2 0
              Point2d.create 3 0 ]
          Left = 0
          Right = 3
          Bottom = 0
          Top = 0 }
    | 1 ->
        { Index = 1
          Points =
            [ Point2d.create 1 0
              Point2d.create 0 1
              Point2d.create 1 1
              Point2d.create 2 1
              Point2d.create 1 2 ]
          Left = 0
          Right = 2
          Bottom = 0
          Top = 2 }
    | 2 ->
        { Index = 2
          Points =
            [ Point2d.create 0 0
              Point2d.create 1 0
              Point2d.create 2 0
              Point2d.create 2 1
              Point2d.create 2 2 ]
          Left = 0
          Right = 2
          Bottom = 0
          Top = 2 }
    | 3 ->
        { Index = 3
          Points =
            [ Point2d.create 0 0
              Point2d.create 0 1
              Point2d.create 0 2
              Point2d.create 0 3 ]
          Left = 0
          Right = 0
          Bottom = 0
          Top = 3 }
    | 4 ->
        { Index = 4
          Points =
            [ Point2d.create 0 0
              Point2d.create 1 0
              Point2d.create 0 1
              Point2d.create 1 1 ]
          Left = 0
          Right = 1
          Bottom = 0
          Top = 1 }
    | _ -> invalidOp "bad index"

let moveRock (dx: int) (dy: int) (rock: Rock) : Rock =
    { rock with
        Points = rock.Points |> List.map (fun p -> p |> Point2d.move dx dy)
        Left = rock.Left + dx
        Right = rock.Right + dx
        Bottom = rock.Bottom + dy
        Top = rock.Top + dy}

let placePoint (value: int) (blocks: int[,]) (p: Point2d) : int[,] =
    blocks[p.X, p.Y] <- value
    blocks
    
let placeRock (rock: Rock) (chamber: Chamber) : Chamber =
    let height = 1 + (rock.Points |> List.map (fun p -> p.Y) |> List.max) 
    let newBlocks = rock.Points |> List.fold (fun b p -> placePoint (rock.Index + 1) b p) (chamber |> expandBlocks height)
    { chamber with Blocks = newBlocks ; Height = max height chamber.Height}

type DropState =
    {
        RockIndex : int
        JetIndex : int
        Top: int[]
    }
    
type ChamberState =
    {
        Count: int
        Height: int
        DropState: DropState
    }
    
type RockState =
    | NoRock
    | Falling

type State =
    { JetIndex: int
      Jets: int list
      RockIndex: int
      RockState: RockState
      Count: int
      Rock: Rock
      Chamber: Chamber
      ChamberStates: ChamberState list }

let startingChamber = { Blocks = Array2D.zeroCreate 7 8; Height = 0 }

let dropNextRock (state: State) : State =
    let rockIndex =
        if state.RockIndex = 4 then
            0
        else
            state.RockIndex + 1
    let y = state.Chamber.Height + 3
    let x = 2
    let rock = getRock rockIndex |> moveRock x y
    
    let top =
        if state.Chamber.Height = 0 then
            Array.create 7 0
        else
            state.Chamber.Blocks[*, state.Chamber.Height - 1]
            
    let chamberState = {
        Count = state.Count + 1
        Height = state.Chamber.Height
        DropState = { RockIndex = rockIndex; JetIndex = state.JetIndex; Top = top }
    }

    { state with
        Count = state.Count + 1
        RockIndex = rockIndex
        Rock = rock
        RockState = Falling
        ChamberStates = [chamberState] |> List.append state.ChamberStates }
    
let collideWithChamber (chamber: Chamber) (rock: Rock) : bool =
    rock.Points |> List.exists (fun p -> chamber.Blocks[p.X, p.Y] > 0)

let updateRock (state: State) : State =
    let jet = state.Jets[state.JetIndex]
    // match jet with
    // | -1 -> printfn "Left"
    // | 1 -> printfn "Right"
    // | _ -> invalidOp "bad jet"

    let newJetIndex =
        if state.JetIndex = (jets.Length - 1) then
            0
        else
            state.JetIndex + 1

    let rock1 = state.Rock |> moveRock jet 0
    
    let rock2 =
        if rock1.Left = -1 || rock1.Right = 7 || rock1 |> collideWithChamber state.Chamber then
            state.Rock
        else
            rock1
            
    let rock3 = rock2 |> moveRock 0 -1
    
    if rock3.Bottom = -1 || rock3 |> collideWithChamber state.Chamber then
        let newChamber = state.Chamber |> placeRock rock2
        { state with JetIndex = newJetIndex; Rock = EmptyRock; RockState = NoRock; Chamber = newChamber }
    else
        // rock2
        { state with JetIndex = newJetIndex; Rock = rock3 }
        
let visualizeState (blocks: int[,]) (rock: Rock) (height: int): unit =
    let w = blocks |> Array2D.length1
    let h = blocks |> Array2D.length2
    let array =
        Array2D.init w h (fun x y -> match blocks[x, y] with |0 -> "." | _ -> string blocks[x,y])
    
    for p in rock.Points do
        array[p.X, p.Y] <- "@"

    printfn "Height: %i" height
    for y = (h - 1) downto 0 do
        printf "%3i|" y
        for x in 0..(w - 1) do
           printf "%s" array[x, y]
        printfn "|"
    printfn "   +-------+"
        
    
let initialState =
    { JetIndex = 0
      Jets = jets
      RockIndex = -1
      RockState = NoRock
      Count = 0
      Rock = EmptyRock
      Chamber = startingChamber
      ChamberStates = []
    }

let maxCount = 8192

let rec RunSimulation (state: State) : State =
    if state.Count > maxCount then
        state
    else
        let newState =
            match state.RockState with
            | NoRock -> dropNextRock state
            | Falling -> updateRock state
        //visualizeState newState.Chamber.Blocks newState.Rock newState.Chamber.Height
        newState |> RunSimulation

let finalState = initialState |> RunSimulation

let startIndex = 512

let startHeight = finalState.ChamberStates[startIndex - 1].Height
let tail = finalState.ChamberStates[startIndex..]
let startState = tail.Head

let nextIndex = (tail.Tail |> List.findIndex (fun cs -> cs.DropState = startState.DropState)) + startIndex + 1

let width = nextIndex - startIndex

let nextState = finalState.ChamberStates[nextIndex - 1]

printfn "%A %A %A" finalState.ChamberStates[startIndex] finalState.ChamberStates[startIndex + width] finalState.ChamberStates[startIndex + 2 * width] 

let deltaCount = int64 nextState.Count - int64 startState.Count
let deltaHeight = int64 nextState.Height - int64 startState.Height

let desiredCount = int64 2022
let periodicCount = desiredCount - int64 startIndex

let periods = (periodicCount / deltaCount)
let remainder = periodicCount % deltaCount

let remainderHeight = tail[startIndex + int remainder].Height - startHeight

let height = int64 finalState.ChamberStates[startIndex].Height + (periods * deltaHeight) // + int64 remainderHeight

printfn "Height: %i" height