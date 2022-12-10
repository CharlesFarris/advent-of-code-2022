namespace Common

type Point2d = { X: int; Y: int }

module Point2d =
    let Zero = { X = 0; Y = 0 }

    let move (dX: int) (dY: int) (p: Point2d) : Point2d = { X = p.X + dX; Y = p.Y + dY }
