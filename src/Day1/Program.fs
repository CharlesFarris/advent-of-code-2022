open System
open System.IO

type ElfCalories = { Id: int; Calories: int }

let data2 =
    [ 1000
      2000
      3000
      0
      4000
      0
      5000
      6000
      0
      7000
      8000
      9000
      0
      10000 ]
    
let parseFun line =
    if String.IsNullOrWhiteSpace(line) then
        0
    else
        Convert.ToInt32(line)
let lines = File.ReadLines ".\part1_data.txt" |> List.ofSeq
let data = List.map parseFun lines 

let elfList: ElfCalories list =
    [ { Id = 1; Calories = 0 } ]

let sumFun (acc: ElfCalories list) (elem: int) =
    let current = acc.Head
    printfn "%i %i %i" current.Id current.Calories elem

    match elem with
    | 0 -> List.append [ { Id = current.Id + 1; Calories = 0 } ] acc
    | _ -> List.append [ { current with Calories = current.Calories + elem } ] acc.Tail

let sumList = List.fold sumFun elfList data

sumList |> List.iter (fun current -> printfn "Id: %i Calories: %i" current.Id current.Calories )

let findMax (acc: ElfCalories ) (elem: ElfCalories) =
    if acc.Calories < elem.Calories then
        elem
    else
        acc

let max = List.fold findMax { Id = 0; Calories = 0 } sumList

printfn "Max: Id: %i Calories: %i" max.Id max.Calories