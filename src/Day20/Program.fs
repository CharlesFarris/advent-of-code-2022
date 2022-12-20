open System.IO
open Day20.Functions

let initial =
    ".\\part1_data.txt"
    |> File.ReadLines
    |> Seq.map int
    |> Seq.toList
    
let mutable stack = initial |> List.append []
let mutable current = initial |> List.append []
//current |> printArrangement
while stack.Length > 0 do
    let head = stack.Head
    stack <- stack.Tail
    current <- head |> mix2 current
    //current |> printArrangement

let index = current |> List.findIndex (fun i -> i = 0)
let x = current |> getValueAt 1000
let y = current |> getValueAt 2000
let z = current |> getValueAt 3000

let sum = x + y + z
printfn "Sum: %i" sum
