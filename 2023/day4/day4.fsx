open System




let calculate (s: string) =
    s
    |> _.Split(": ")
    |> fun x -> x[1]
    |> _.Split(" | ")
    |> Array.map (_.Split(" ", (StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)))
    |> fun x ->
        x[0]
        |> Array.filter (fun n -> x[1] |> Array.contains n)
        |> Array.length
        |> fun x -> if x > 0 then 2.0 ** (float x - 1.0) else 0
        |> int
    


let main () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map calculate
    |> Array.sum
    |> fun x -> printfn $"Answer: {x}"
    
    
    
main()