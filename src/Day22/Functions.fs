module Day22.Functions

open Day22.Types

let parseMap (lines: string list) : Map =
    { Width = 0
      Height = 0 }
    
let parseOrders (lines: string list) : Order list =
    let line = lines |> List.last
    []