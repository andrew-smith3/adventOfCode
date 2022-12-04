open System

let findCommonItem (line: string) =
    let half = line.Length / 2
    let first = line.Substring(0, half)
    let second = line.Substring(half)
    first
    |> Seq.find second.Contains

let getPriority (c: char) =
    match c with
    | x when Char.IsLower(x) -> (int x) - 96
    | x -> (int x) - 38

let accumulatePriority (p : int) (line: string) =
    line
    |> findCommonItem
    |> getPriority
    |> (+) p

let part1 () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.fold accumulatePriority 0
    |> printfn "Answer: %d"
    
part1 ()