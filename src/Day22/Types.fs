namespace Day22.Types

open Geometry

type Order =
    | Move
    | RotateLeft
    | RotateRight

type Map =
    { Width: int
      Height: int
      Values: char[,] }

type Facing =
    | Up
    | Right
    | Down
    | Left

type State =
    { Map: Map
      Orders: Order list
      Cursor: Point2d
      Facing: Facing }
