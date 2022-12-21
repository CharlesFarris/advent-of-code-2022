namespace Day21.Types

type Operation =
    | Add
    | Subtract
    | Multiply
    | Divide

type Monkey = { Id: string }

type OperationJob =
    { Monkey: string
      LeftMonkey: string
      RightMonkey: string
      Operation: Operation }

type PartialJob =
    { Monkey: string
      LeftMonkey: string
      Left: int64 option
      RightMonkey: string
      Right: int64 option
      Operation: Operation
      Number: int64 option }

type Job =
    | O of OperationJob
    | P of PartialJob

type State =
    { Numbers: Map<string, int64>
      Jobs: Job list }
