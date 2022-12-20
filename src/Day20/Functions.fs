module Day20.Functions

let getValueAt (offset: int) (current: int list) : int =
    let zeroIndex = current |> List.findIndex (fun i -> i = 0)
    current[ (zeroIndex + offset) % current.Length ]

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
            
let mix2 (current: int list) (value: int) : int list =
    if value = 0 then
        current
    else
        let index = current |> List.findIndex (fun i -> i = value)
        if value > 0 then
            let absoluteIndex = index + value
            let relativeIndex = absoluteIndex % current.Length
            let targetValue = current[ relativeIndex ]
            let c1 = current |> List.removeAt index        
            let newIndex = 1 + (c1 |> List.findIndex (fun i -> i = targetValue))
            c1 |> List.insertAt newIndex value
        else
            let absoluteIndex = index + value
            let relativeIndex =
                if absoluteIndex > 0 then
                    absoluteIndex % current.Length
                else
                    absoluteIndex % current.Length + current.Length
            let targetValue = current[ relativeIndex - 1]
            let c1 = current |> List.removeAt index        
            let newIndex = 1 + (c1 |> List.findIndex (fun i -> i = targetValue))
            c1 |> List.insertAt newIndex value
    
let printArrangement (current: int list) : unit =
    for i in current do
        printf "%i " i
    printfn ""
    