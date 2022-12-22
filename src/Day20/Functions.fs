module Day20.Functions

let getRelativeIndex (absoluteIndex: int) (arrangement: (int * int) list) : int =
    let remainder = absoluteIndex % arrangement.Length
    if remainder < 0 then
        remainder + arrangement.Length
    else
        remainder
    
let getValueAt (absoluteIndex: int) (arrangement: (int * int) list) : int * int =
    let relativeIndex = arrangement |> getRelativeIndex absoluteIndex
    arrangement[relativeIndex]

let mix (tuple: int * int) (arrangement: (int * int) list) : (int * int) list =
    let value = snd tuple
    if value = 0 then
        arrangement
    else
        let valueIndex =
            arrangement
            |> List.findIndex (fun i -> i = tuple)
        let absoluteIndex =
            if value >= 0 then 
                valueIndex + value
            else
                valueIndex + value - 1
        let insertAfterValue =
            arrangement
            |> getValueAt absoluteIndex
        if insertAfterValue = tuple then
            arrangement
        else
            let temporaryArrangement =
                arrangement |> List.removeAt valueIndex

            let insertAfterValueIndex =
                temporaryArrangement
                |> List.findIndex (fun i -> i = insertAfterValue)
            temporaryArrangement
                |> List.insertAt (insertAfterValueIndex + 1) tuple
    
let printArrangement (current: (int * int) list) : unit =
    for i in current do
        printf "%i " (snd i)
    printfn ""
    