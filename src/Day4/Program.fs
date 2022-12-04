open System.IO

let sectionAssignments =
    File.ReadLines ".\part1_data.txt" |> List.ofSeq
   
type Range = {
    Start: int
    End: int
}

let fullyContains (left:Range ) (right:Range) =
    (left.Start <= right.Start && right.End <= left.End) || (right.Start <= left.Start && left.End <= right.End)
    
let intersect (left:Range) (right: Range) =
    (left.End >= right.Start) && (right.End >= left.Start);
    

let parseRange (line:string) =
    let tokens = line.Split '-'
    {Start = (int tokens[0]); End = (int tokens[1])}
    
let parseLine (line:string) =
    let tokens = line.Split ','
    let left = parseRange tokens[0]
    let right = parseRange tokens[1]
    [left; right]
    
let checkAssignments (sum: int) (line:string) =
    let ranges = parseLine line
    if intersect ranges[0] ranges[1] then
        sum + 1
    else
        sum

let count = List.fold checkAssignments 0 sectionAssignments
printfn "Count: %i" count