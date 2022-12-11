open System.IO
open Microsoft.FSharp.Core

let instructions = ".\\part1_data.txt" |> File.ReadAllLines |> Seq.toList

let chunks = instructions |> List.chunkBySize 7

type Operator =
    | Addition
    | Multiplication

type Operation =
    { Left: string
      Right: string
      Operator: Operator }

type Header =
    { Id: int
      Left: string
      Right: string
      Operation: Operator
      Divisor: decimal
      TrueTarget: int
      FalseTarget: int }

type Monkey =
    { Header: Header
      Items: decimal list
      Inspections: int }

let CatchItem (item: decimal) (monkey: Monkey) : Monkey =
    { monkey with Items = [item ] |> List.append monkey.Items }

let ThrowItem (tuple: decimal * int) (monkeys: Monkey list) : Monkey list =
    let item = fst tuple
    let target = snd tuple
    [ for monkey in monkeys ->
          if monkey.Header.Id = target then
              CatchItem item monkey
          else
              monkey ]

let ClearItems (target: int) (monkeys: Monkey list) : Monkey list =
    [ for monkey in monkeys ->
          if monkey.Header.Id = target then
              { monkey with Items = [] }
          else
              monkey ]

let parseChunk (acc: Monkey list) (chunk: string list) : Monkey list =
    let simplifiedChunk = chunk |> List.map (fun l -> l[l.LastIndexOf(':') + 1 ..])

    let startingItems =
        simplifiedChunk[ 1 ].Split [| ',' |] |> Array.toList |> List.map decimal

    let tokens = (simplifiedChunk[2][7..]).Split [| ' ' |]
    let left = tokens[0]
    let right = tokens[2]

    let operation =
        match tokens[1] with
        | "+" -> Addition
        | "*" -> Multiplication
        | _ -> invalidOp "Unknown operation"

    let divisor = decimal (simplifiedChunk[3][13..])
    let trueTarget = int (simplifiedChunk[4][16..])
    let falseTarget = int (simplifiedChunk[5][16..])

    acc
    |> List.append
        [ { Header =
              { Id = acc.Length
                Left = left
                Right = right
                Operation = operation
                Divisor = divisor
                TrueTarget = trueTarget
                FalseTarget = falseTarget }
            Items = startingItems
            Inspections = 0 } ]

let monkeys = chunks |> List.fold parseChunk [] |> List.rev

let lcd = monkeys |> List.map (fun m -> m.Header.Divisor) |> List.reduce (fun acc elem -> acc * elem)

type State =
    { Round: int
      CurrentMonkey: int
      Monkeys: Monkey list }

let initialState =
    { Round = 0
      CurrentMonkey = 0
      Monkeys = monkeys }

let evaluateItem (item: decimal) (header: Header) : decimal * int =
    let leftValue =
        match header.Left with
        | "old" -> item
        | _ -> decimal header.Left

    let rightValue =
        match header.Right with
        | "old" -> item
        | _ -> decimal header.Right

    let inspectionLevel =
        match header.Operation with
        | Addition -> leftValue + rightValue
        | Multiplication -> leftValue * rightValue
    let newItem = inspectionLevel % lcd  

    let remainder = newItem % header.Divisor   
    if remainder = 0M then
        (newItem, header.TrueTarget)
    else
        (newItem,header.FalseTarget)

let rounds = [ for i in 1..10000 -> i ]

let handleTurn (monkeys: Monkey list) (current: int) : Monkey list =
    let monkey = monkeys[current]
    let items = monkey.Items
    let inspections = monkey.Inspections + items.Length
    let currentMonkeys = [ for m in monkeys -> if m.Header.Id = current then { m with Items=[]; Inspections=inspections } else m ]
    let handleItem (localMonkeys: Monkey list) (item: decimal) : Monkey list =
        let tuple = monkey.Header |> evaluateItem item
        localMonkeys |> ThrowItem tuple
    items |> List.fold handleItem currentMonkeys 
    
let handleRound (state: State) (round: int) =
    let newMonkeys = [0..(state.Monkeys.Length - 1)] |> List.fold handleTurn state.Monkeys
    let newState = { state with Monkeys = newMonkeys }
    printfn "Round: %i" round
    for m in newState.Monkeys do
        (printfn "Monkey %i: %i %A" m.Header.Id m.Inspections m.Items)
    newState
    
let finalState = rounds |> List.fold handleRound initialState

let inspections = finalState.Monkeys |> List.map (fun m -> m.Inspections) |> List.sort |> List.rev

let product = (decimal inspections[0]) * (decimal inspections[1])
printfn "Monkey Business: %M" product
