
let count ((ones, threes) : int * int) ((l, g) : int * int) = 
    match g - l with
    | 1 -> (ones+1, threes)
    | 3 -> (ones, threes + 1)
    | x -> failwithf "Unhandled: %d %d %d" g l x


let main () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map int
    |> Seq.append (seq {0})
    |> Seq.sort
    |> Seq.pairwise
    |> Seq.fold count (0, 0)
    |> fun (ones, threes) -> ones * (threes + 1)
    |> printfn "Answer: %d"

main ()