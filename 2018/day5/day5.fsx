open System;

let readInputFile () = System.IO.File.ReadAllText("./2018/day5/input.txt") 

let charsCancel (a: char) (b: char) = a <> b && (Char.ToLower a) = (Char.ToLower b)

let folder (state: char list) (c: char) : char list = 
    if List.isEmpty state 
    then c::state 
    else
        match (charsCancel c (List.head state)) with
        | true -> List.tail state
        | false -> c::state

let processUnits (s: string) = 
    Seq.fold folder [] s 
    |> List.length

let main() = 
    readInputFile()
    |> processUnits
    |> printfn "%i" 