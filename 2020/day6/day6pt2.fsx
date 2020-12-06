let countQuestions (lines: string list) = 
    lines 
    |> Seq.concat 
    |> Seq.distinct
    |> Seq.where (fun x -> List.forall (Seq.contains x) lines)
    |> Seq.length

let fold ((sum, current): int * string list) (line: string) =
    match line with
    | "" -> 
        let count = countQuestions current
        (sum + count, [])
    | x ->
        let current = line::current
        (sum , current)

let main () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.fold fold (0, [])
    |> fun (sum , current) -> sum + countQuestions current
    |> printfn "Answer: %d"


main ()