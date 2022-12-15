module Geometry

open Range

type Offset2d = { DeltaX: int; DeltaY: int }

type Point2d = { X: int; Y: int }

module Point2d =
    let Zero = { X = 0; Y = 0 }

    let move (dX: int) (dY: int) (p: Point2d) : Point2d = { X = p.X + dX; Y = p.Y + dY }

    let offset (p1 : Point2d) (p2 : Point2d) : Offset2d =
        { DeltaX = p2.X - p1.X; DeltaY = p2.Y - p1.Y }
    
    let manhattanDistance (p1: Point2d) (p2: Point2d ) : int =
        let offset = offset p1 p2
        abs(offset.DeltaX) + abs(offset.DeltaY)

    let solveManhattanDistance (p1: Point2d) (y: int) (d: int) : Range1d option =
        let dy = abs(p1.Y - y)
        if dy <= d then
            Some (Range1d.create (p1.X - d + dy) (p1.X + d - dy )) 
        else
            None
        