
let sumCalories ((elves, current) : int list * int) (line: string) =
    match line with
    | "" -> (current::elves, 0)
    | x -> (elves, current + (int x))

let part1 () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.fold sumCalories ([], 0)
    |> fst
    |> List.max
    |> printfn "Answer: %d"
    
// part1 ()


let part2 () =
    System.IO.File.ReadAllLines("testInput.txt")
    |> Array.fold sumCalories ([], 0)
    |> fst
    |> fun x ->
        printfn "%A" x
        x
    |> List.sortDescending
    |> List.take 3
    |> List.sum
    |> printfn "Answer: %d"
    
part2()