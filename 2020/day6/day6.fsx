let countQuestions (line: string) = 
    line
    |> Seq.distinct
    |> Seq.length

let fold ((sum, current): int * string) (line: string) =
    match line with
    | "" -> 
        let count = countQuestions current
        (sum + count, "")
    | x ->
        let current = current + line
        (sum , current)

let main () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.fold fold (0, "")
    |> fun (sum , current) -> sum + countQuestions current
    |> printfn "Answer: %d"


main ()