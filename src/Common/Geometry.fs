module Geometry

type Offset2d = { DeltaX: int; DeltaY: int }

type Point2d = { X: int; Y: int }

module Point2d =
    let Zero = { X = 0; Y = 0 }

    let move (dX: int) (dY: int) (p: Point2d) : Point2d = { X = p.X + dX; Y = p.Y + dY }

module Offset2d =
    /// <summary>
    /// Computes the offset from p1 to p2.
    /// </summary>
    let compute (p1 : Point2d) (p2 : Point2d) : Offset2d =
        { DeltaX = p2.X - p1.X; DeltaY = p2.Y - p1.Y }
