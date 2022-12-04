open System

let findCommonItem (group: string[]) =
    let first = group[0]
    let second = group[1]
    let third = group[2]
    first
    |> Seq.find (fun x -> second.Contains(x) && third.Contains(x))

let getPriority (c: char) =
    match c with
    | x when Char.IsLower(x) -> (int x) - 96
    | x -> (int x) - 38

let accumulatePriority (p : int) (group: string[]) =
    group
    |> findCommonItem
    |> getPriority
    |> (+) p

let part2 () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.chunkBySize 3
    |> Array.fold accumulatePriority 0
    |> printfn "Answer: %d"
    
part2 ()