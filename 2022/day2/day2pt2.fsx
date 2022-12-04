let calculateScore (opMove: char) (roundOutcome: char) =
    let yourMove =
        match opMove, roundOutcome with
        | 'A', 'X' -> 'Z'
        | 'B', 'X' -> 'X'
        | 'C', 'X' -> 'Y'
        | 'A', 'Y' -> 'X'
        | 'B', 'Y' -> 'Y'
        | 'C', 'Y' -> 'Z'
        | 'A', 'Z' -> 'Y'
        | 'B', 'Z' -> 'Z'
        | 'C', 'Z' -> 'X'
        | _, _ -> failwith "Invalid"
    
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

let main () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.fold accumulateScore 0
    |> printfn "Answer: %d"
    
main ()