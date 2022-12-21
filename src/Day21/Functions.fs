module Day21.Functions

open Day21.Types

let parseLine (data: State) (line: string) : State =
    let tokens = line.Split [| ':'; ' ' |]

    match tokens.Length with
    | 3 ->
        let newNumbers = data.Numbers |> Map.add tokens[0] (int tokens[2])

        { data with Numbers = newNumbers }
    | _ ->
        let operation =
            match tokens[3] with
            | "*" -> Multiply
            | "+" -> Add
            | "-" -> Subtract
            | "/" -> Divide
            | _ -> invalidOp "invalid operation"

        let job =
            O
                { Monkey = tokens[0]
                  LeftMonkey = tokens[2]
                  Operation = operation
                  RightMonkey = tokens[4] }

        { data with Jobs = [ job ] |> List.append data.Jobs }

let doOperation (operation: Operation) (left: int) (right: int) =
    match operation with
    | Add -> left + right
    | Subtract -> left - right
    | Multiply -> left * right
    | Divide -> left / right

let updateJob (numbers: Map<string, int>) (job: Job) : Job =
    match job with
    | O o ->
        let leftResult = numbers |> Map.tryFind o.LeftMonkey
        let rightResult = numbers |> Map.tryFind o.RightMonkey

        match leftResult, rightResult with
        | Some l, Some r ->
            P
                { Monkey = o.Monkey
                  Operation = o.Operation
                  LeftMonkey = o.LeftMonkey
                  Left = Some l
                  RightMonkey = o.RightMonkey
                  Right = Some r
                  Number = Some(doOperation o.Operation l r) }
        | Some l, None ->
            P
                { Monkey = o.Monkey
                  Operation = o.Operation
                  LeftMonkey = o.LeftMonkey
                  Left = Some l
                  RightMonkey = o.RightMonkey
                  Right = None
                  Number = None }
        | None, Some r ->
            P
                { Monkey = o.Monkey
                  Operation = o.Operation
                  LeftMonkey = o.LeftMonkey
                  Left = None
                  RightMonkey = o.RightMonkey
                  Right = Some r
                  Number = None }
        | None, None -> O o
    | P p ->
        match p.Left, p.Right with
        | Some l, None ->
            let result = numbers |> Map.tryFind p.RightMonkey

            match result with
            | Some r ->
                P
                    { Monkey = p.Monkey
                      Operation = p.Operation
                      LeftMonkey = p.LeftMonkey
                      Left = p.Left
                      RightMonkey = p.RightMonkey
                      Right = Some r
                      Number = Some(doOperation p.Operation l r) }
            | None -> P p
        | None, Some r ->
            let result = numbers |> Map.tryFind p.LeftMonkey

            match result with
            | Some l ->
                P
                    { Monkey = p.Monkey
                      Operation = p.Operation
                      LeftMonkey = p.LeftMonkey
                      Left = Some l
                      RightMonkey = p.RightMonkey
                      Right = p.Right
                      Number = Some(doOperation p.Operation l r) }
            | None -> P p
        | None, None -> P p

let isJobComplete (job: Job) : bool =
    match job with
    | P p -> p.Number |> Option.isSome
    | _ -> false

let mapCompletedJob (job: Job) : string * int =
    match job with
    | P p ->
        match p.Number with
        | Some v -> p.Monkey, v
        | None -> invalidOp "bad job"
    | _ -> invalidOp "bad job"

let updateJobs (state: State) : State =
    let updatedJobs = state.Jobs |> List.map (updateJob state.Numbers)
    let newJobs = updatedJobs |> List.where (fun j -> not (j |> isJobComplete))
    let completed = updatedJobs |> List.where isJobComplete |> List.map mapCompletedJob

    let newNumbers =
        completed |> List.fold (fun m t -> m |> Map.add (fst t) (snd t)) state.Numbers

    { state with
        Numbers = newNumbers
        Jobs = newJobs }

let printJob (job: Job) : unit =
    match job with
    | O o -> printfn "%s: %s %A %s" o.Monkey o.LeftMonkey o.Operation o.RightMonkey
    | P p ->
        let left =
            match p.Left with
            | Some v -> string v
            | None -> p.LeftMonkey

        let right =
            match p.Right with
            | Some v -> string v
            | None -> p.RightMonkey

        printfn "%s: %s %A %s" p.Monkey left p.Operation right

let printState (state: State) : unit =
    state.Numbers
    |> Map.toList
    |> List.sortBy fst
    |> List.iter (fun t -> printfn "%s: %i" (fst t) (snd t))

    state.Jobs |> List.iter (fun j -> j |> printJob)
    printfn ""

let rec runSimulation (state: State) : State =
    state |> printState

    if state.Numbers |> Map.containsKey "root" then
        state
    else
        state |> updateJobs |> runSimulation
