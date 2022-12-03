open System.IO
    
let rucksacks =
    File.ReadLines ".\part1_data.txt" |> List.ofSeq

let computePriority (c: char) : int =
    if ('a' <= c && c <= 'z') then
        int c - 97 + 1
    elif ('A' <= c && c <= 'Z') then
        int c - 65 + 27
    else
        0

let analyzeCompartments (sum: int) (line: string) =
    let size = line.Length / 2
    let left = line[.. (size - 1)]
    let right = line[size..]
    printfn "%s %s %s" line left right

    let matches =
        String.filter right.Contains left |> Seq.toList |> List.distinct

    List.iter (fun c -> printfn "%c" c) matches

    List.fold (fun newSum c -> newSum + (computePriority c)) sum matches

let sum =
    List.fold analyzeCompartments 0 rucksacks

printfn "Sum: %i" sum
