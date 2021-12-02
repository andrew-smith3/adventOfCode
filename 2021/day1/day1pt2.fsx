let readFile () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map int
    
let main () =
    readFile ()
    |> Seq.windowed 3
    |> Seq.map Array.sum
    |> Seq.pairwise
    |> Seq.fold (fun s (x, y) -> if x < y then s + 1 else s) 0
    |> fun x -> printfn "Answer: %d" x
    
main ()