let parseLines (s: string) =
    s.Split ' '
    |> (fun x -> x.[0], (int x.[1]))

let readFile () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map parseLines
    
    
let processCommands ((h, d, a): int * int * int) ((command, amount): string * int) =
    match command with
    | "forward" -> (h + amount, d + (a * amount), a)
    | "down" -> (h, d, a + amount)
    | "up" -> (h, d, a - amount)
    | _ -> (h, d, a)
    
let main () =
    readFile ()
    |> Seq.fold processCommands (0, 0, 0)
    |> fun (h, d, _) -> h * d
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()
    
   