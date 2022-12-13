open System
open System.IO

type ItemInteger = ItemInteger of int

type ItemList = ItemList of int list

type ItemString = ItemString of value: string

type Item =
    | ItemInteger of int
    | ItemList of Item list
    | ItemString of string
    | Item

type Packet = { Left: Item; Right: Item }

type ParsingState =
    | StateNone
    | StateNumber
    | StateItem

type Parser =
    { State: ParsingState
      BracketDepth: int
      Current: string
      Values: string list }

let rec parseItem (line: string) : Item =
    let parseCharacter (parser: Parser) (c: char) : Parser =
        match parser.State with
        | StateNone ->
            if c = ',' then
                parser
            elif c = ']' then
                invalidOp "Bad state"
            elif c = '[' then
                { parser with
                    State = StateItem
                    Current = parser.Current + string c
                    BracketDepth = 1 }
            elif Char.IsDigit c then
                { parser with
                    State = StateNumber
                    Current = parser.Current + "#" + string c }
            else
                invalidOp "Unrecognized character"
        | StateNumber ->
            if c = ',' || c = ']' then
                { parser with
                    State = StateNone
                    Values = [ parser.Current ] |> List.append parser.Values
                    Current = "" }
            elif Char.IsDigit c then
                { parser with Current = parser.Current + string c }
            elif c = '[' then
                invalidOp "Bad state"
            else
                invalidOp "Unrecognized character"
        | StateItem ->
            if c = ']' then
                match parser.BracketDepth with
                | 0 -> invalidOp "Bad bracket depth"
                | 1 ->
                    let current = parser.Current + string c

                    { parser with
                        State = StateNone
                        Values = [ current ] |> List.append parser.Values
                        Current = "" }
                | _ ->
                    { parser with
                        BracketDepth = parser.BracketDepth - 1
                        Current = parser.Current + string c }
            elif c = '[' then
                { parser with
                    BracketDepth = parser.BracketDepth + 1
                    Current = parser.Current + string c }
            elif Char.IsDigit c || c = ',' then
                { parser with Current = parser.Current + string c }
            else
                invalidOp "Unrecognized character"

    let initialParser =
        { State = StateNone
          BracketDepth = 0
          Current = ""
          Values = [] }

    let content = line[1 .. line.Length - 2]
    let finalParser = content |> Seq.toList |> List.fold parseCharacter initialParser

    let values =
        match finalParser.State with
        | StateNone -> finalParser.Values
        | StateItem -> invalidOp "Bad item state"
        | StateNumber -> [ finalParser.Current ] |> List.append finalParser.Values

    //printfn "%A" finalParser

    let item =
        ItemList(
            [ for value in values ->
                  if value = "" then ItemList([])
                  elif value.StartsWith "[" then parseItem value
                  elif value.StartsWith "#" then ItemInteger(int value[1..])
                  else invalidOp "Bad Value" ]
        )

    //printfn "%A" item
    item

type OrderState =
    | Indeterminate
    | Correct
    | Incorrect

let rec compareItems (left: Item) (right: Item) : OrderState =
    match left, right with
    | ItemInteger leftInt, ItemInteger rightInt ->
        if leftInt < rightInt then Correct
        elif leftInt = rightInt then Indeterminate
        else Incorrect
    | ItemInteger _, ItemList _ -> compareItems (ItemList [ left ]) right
    | ItemList _, ItemInteger _ -> compareItems left (ItemList[right])

    | ItemList leftList, ItemList rightList ->
        let length = max leftList.Length rightList.Length

        let ll =
            if leftList.Length < length then
                [ for i in 1 .. (length - leftList.Length) -> None ]
                |> List.append (leftList |> List.map Some)
            else
                leftList |> List.map Some

        let rl =
            if rightList.Length < length then
                [ for i in 1 .. (length - rightList.Length) -> None ]
                |> List.append (rightList |> List.map Some)
            else
                rightList |> List.map Some

        let rec compareItemsOptional (left: Item option) (right: Item option) : OrderState =
            match left, right with
            | Some l, Some r -> compareItems l r
            | None, Some _ -> Correct
            | Some _, None -> Incorrect
            | None, None -> invalidOp "Invalid state"

        let results = List.map2 compareItemsOptional ll rl
        let correctIndex = results |> List.tryFindIndex (fun r -> r = Correct)
        let incorrectIndex = results |> List.tryFindIndex (fun r -> r = Incorrect)

        match correctIndex, incorrectIndex with
        | Some ci, Some ii -> if ci < ii then Correct else Incorrect
        | Some ci, None -> Correct
        | None, Some ii -> Incorrect
        | None, None -> Indeterminate

    | _, _ -> invalidOp "Not implemented"

let checkPacketOrder (tuple: int * Packet) : OrderState =
    let index = fst tuple
    let packet = snd tuple
    compareItems packet.Left packet.Right

let lines =
    ".\\part1_data.txt"
    |> File.ReadAllLines
    |> Seq.toList
    |> List.filter (fun l -> not (String.IsNullOrWhiteSpace(l)))

let key1 = "[[2]]" |> parseItem
let key2 = "[[6]]" |> parseItem

let items = lines |> List.map parseItem |> List.append [ key1; key2 ]

let comparer (left: Item) (right: Item) : int =
    match compareItems left right with
    | Correct -> -1
    | Incorrect -> 1
    | Indeterminate -> 0

let sortedItems = items |> List.sortWith comparer

let index1 = sortedItems |> List.findIndex (fun i -> i = key1)
let index2 = sortedItems |> List.findIndex (fun i -> i = key2)
let decoderKey = (index1 + 1) * (index2 + 1)

printfn "%i" decoderKey
