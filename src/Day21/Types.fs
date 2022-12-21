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
      Left: int option
      RightMonkey: string
      Right: int option
      Operation: Operation
      Number: int option }

type Job =
    | O of OperationJob
    | P of PartialJob

type State =
    { Numbers: Map<string, int>
      Jobs: Job list }
