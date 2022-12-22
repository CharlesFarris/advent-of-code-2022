namespace Day22.Types

type Map = {
    Width: int
    Height: int
}

type Order =
    | Move of int
    | Left
    | Right

