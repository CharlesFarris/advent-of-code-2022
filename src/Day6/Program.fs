let signal = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

let chunks = [ for i in 0..(signal.Length - 4) -> signal[i..(i+3)] ] 

let index = chunks |> List.findIndex (fun chunk -> (chunk |> Seq.toList |> List.distinct).Length = 4)

printfn "Index: %i" (index + 4)