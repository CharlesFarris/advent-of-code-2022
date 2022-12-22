open System.IO

let lines =
    ".\\test_data.txt"
    |> File.ReadAllLines
    |> Seq.toList
    
