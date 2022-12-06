open System.IO

let signal = ".\part1_data.txt" |> File.ReadAllText 

let chunks = [ for i in 0..(signal.Length - 14) -> signal[i..(i+13)] ] 

let index = chunks |> List.findIndex (fun chunk -> (chunk |> Seq.toList |> List.distinct).Length = 14)

printfn "Index: %i" (index + 14)