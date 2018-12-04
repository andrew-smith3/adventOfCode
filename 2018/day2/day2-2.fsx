open System;

let readInputFile () = 
    System.IO.File.ReadAllLines("./2018/day2/input.txt") 
    |> Array.toList


let isOffBy1 (s1: string) (s2: string) : bool = 
    let diff = Seq.zip s1 s2 |> Seq.filter (fun (a, b) -> a <> b)
    (Seq.length diff) = 1

let findOffByOne (input: string list) = 
    let rec helper (strings: string list) = 
        match strings with
        | [] -> failwith "None found"
        | h::t -> 
            let filter = isOffBy1 h
            let matches = input |> List.filter filter
            if List.length matches = 1
            then (h, List.head matches)
            else helper t
    helper input  

let commonLetters ((a, b): string * string) = 
    Seq.zip a b
    |> Seq.filter (fun (c1, c2) -> c1 = c2)
    |> Seq.map fst
    |> String.Concat

let main() = 
    readInputFile()
    |> findOffByOne
    |> commonLetters
    |> (fun x -> printfn "Answer is: %s" x)