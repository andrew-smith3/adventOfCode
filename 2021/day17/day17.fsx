let targetYLowerBound = 97

let rec factorial (n: int) =
    match n with
    | 1 -> 1
    | n -> n + (factorial (n - 1))
    
let main () =
    factorial (targetYLowerBound - 1)
    |> fun x -> printfn $"Answer: %A{x}"
    
main ()