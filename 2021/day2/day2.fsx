let parseLines (s: string) =
    s.Split ' '
    |> (fun x -> x.[0], (int x.[1]))

let readFile () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map parseLines
    
    
let processCommands ((h, d): int * int) ((command, amount): string * int) =
    match command with
    | "forward" -> (h + amount, d)
    | "down" -> (h, d + amount)
    | "up" -> (h, d - amount)
    | _ -> (h, d)
    
let main () =
    readFile ()
    |> Seq.fold processCommands (0, 0)
    |> fun (h, d) -> h * d
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()
    
   