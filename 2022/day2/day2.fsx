
let calculateScore (opMove: char) (yourMove: char) =
    let moveScore =
        match yourMove with
        | 'X' -> 1
        | 'Y' -> 2
        | 'Z' -> 3
        | _ -> failwith "Invalid"
    let roundScore = 
        match opMove, yourMove with
        | 'A', 'Y' -> 6
        | 'B', 'Z' -> 6
        | 'C', 'X' -> 6
        | 'A', 'X' -> 3
        | 'B', 'Y' -> 3
        | 'C', 'Z' -> 3
        | _, _ -> 0
        
    moveScore + roundScore

let accumulateScore (score: int) (line: string) =
    calculateScore (line.Chars 0) (line.Chars 2)
    |> (+) score

let part1 () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.fold accumulateScore 0
    |> printfn "Answer: %d"
    
part1 ()