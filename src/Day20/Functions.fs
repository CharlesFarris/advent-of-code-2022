module Day20.Functions

let mix (current: int list) (value: int) : int list =
    if value = 0 then
        current
    else
        let index = current |> List.findIndex (fun i -> i = value)
        let c1 = current |> List.removeAt index
        if value > 0 then
            let absoluteIndex = index + value
            let relativeIndex = absoluteIndex % current.Length
            let newIndex =
                if absoluteIndex < current.Length then
                    relativeIndex
                else
                    relativeIndex + 1
            let c2 = c1 |> List.insertAt newIndex value
            c2
        else
            let absoluteIndex = index + value - 1
            let relativeIndex = absoluteIndex % current.Length
            let newIndex =
                if absoluteIndex < 0 then
                    relativeIndex + current.Length
                else
                    relativeIndex
            let c2 = c1 |> List.insertAt newIndex value
            c2
    
let printArrangement (current: int list) : unit =
    for i in current do
        printf "%i " i
    printfn ""