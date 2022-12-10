namespace Common

type Range = { Start: int; End: int }

module Range =
    /// <summary>
    /// Creates an ordered range.
    /// </summary>
    let create (s: int) (e: int) : Range =
        if s <= e then
            { Start = s; End = e }
        else
            { Start = e; End = s }

    /// <summary>
    /// Orders a range.
    /// </summary>
    let order (r1: Range) = create r1.Start r1.End
    
    /// <summary>
    /// Checks if a range is ordered.
    /// </summary>
    let isOrdered (r1: Range): bool =
        r1.Start <= r1.End

    /// <summary>
    /// Empty range.
    /// </summary>
    let Empty = { Start = 0; End = 0 }

    /// <summary>
    /// Returns the union of 2 ranges.
    /// </summary>
    let union (r1: Range) (r2: Range) : Range =
        let or1 = order r1
        let or2 = order r2

        { Start = min or1.Start or2.Start
          End = max or1.End or2.End }
        
    /// <summary>
    /// Returns the union of a range and a value.
    /// </summary>
    let unionValue (v: int) (r1: Range) : Range =
        union r1 (create v v)

    /// <summary>
    /// Checks if the value is contained inside the range.
    /// </summary>
    let containsValue (v: int) (r1: Range) : bool =
        let or1 = order r1
        or1.Start <= v && v <= or1.End

    /// <summary>
    /// Checks if the second range is contained in the first range.
    /// </summary>
    let containsRange (r1: Range) (r2: Range) : bool =
        let or1 = order r1
        let or2 = order r2
        or1.Start <= or2.Start && or2.End <= or1.End

    /// <summary>
    /// Computes the intersection of two ranges.
    /// </summary>
    let intersect (r1: Range) (r2: Range) : Range option =
        let or1 = order r1
        let or2 = order r2
        let s = max or1.Start or2.Start
        let e = min or1.End or2.End
        if s <= e then Some { Start = s; End = e } else None

    /// <summary>
    /// Checks if two ranges intersect.
    /// </summary>
    let isIntersecting (r1: Range) (r2: Range) : bool =
        match (intersect r1 r2) with
        | Some _ -> true
        | None _ -> false

    /// <summary>
    /// Shifts a range.
    /// </summary>
    let shift (offset: int) (r1: Range) : Range =
        { Start = r1.Start + offset
          End = r1.End + offset }
        