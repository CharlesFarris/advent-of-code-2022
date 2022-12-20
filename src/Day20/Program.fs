open System.IO
open Day20.Functions

let initial =
    ".\\test_data.txt"
    |> File.ReadLines
    |> Seq.map int
    |> Seq.toList
    
let mutable stack = initial |> List.append []
let mutable current = initial |> List.append []
current |> printArrangement
while stack.Length > 0 do
    let head = stack.Head
    stack <- stack.Tail
    current <- head |> mix current
    current |> printArrangement

let index = current |> List.findIndex (fun i -> i = 0)
let x = current[ (index + 1000) % current.Length ]
let y = current[ (index + 2000) % current.Length ]
let z = current[ (index + 3000) % current.Length ]

let sum = x + y + z
printfn "Sum: %i" sum
