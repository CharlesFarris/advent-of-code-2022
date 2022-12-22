open System.IO
open Day20.Functions

let initial =
    ".\\test_data.txt"
    |> File.ReadLines
    |> Seq.mapi (fun i v -> i, int v)
    |> Seq.toList
    
let distinct =
    initial
    |> List.distinct
    
let mutable stack = initial |> List.append []
let mutable current = initial |> List.append []
current |> printArrangement
while stack.Length > 0 do
    let head = stack.Head
    stack <- stack.Tail
    current <- current |> mix head
    current |> printArrangement

let index = current |> List.findIndex (fun i -> (snd i) = 0)
let x = current |> getValueAt (1000 + index)
let y = current |> getValueAt (2000 + index)
let z = current |> getValueAt (3000 + index)

let sum = (snd x) + (snd y) + (snd z)
printfn "Sum: %i" sum
