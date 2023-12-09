open System

let redLimit = 12
let greenLimit = 13
let blueLimit = 14

let isUnderLimit (parts : string array) =
    let num = parts[0] |> Int32.Parse
    match parts[1] with
    | "red" -> num <= redLimit
    | "green" -> num <= greenLimit
    | "blue" -> num <= blueLimit
    | _ -> failwithf "invalid color"

let parseRound (s: string) =
    s.Split(", ")
    |> Array.map _.Split(" ")
    |> Array.forall isUnderLimit

let parse (sum: int) (s: string) =
    let parts = s.Split(": ")
    let game = parts[0].Replace("Game ", "") |> Int32.Parse
    let isPossible =
        parts[1].Split("; ")
        |> Array.forall parseRound
    if isPossible then sum + game else sum   
    
let main () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.fold parse 0
    |> fun x -> printfn $"Answer: {x}"
    
main()