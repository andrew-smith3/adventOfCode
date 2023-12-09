let targetXLowerBound = 241
let targetXUpperBound = 273
let targetYLowerBound = 63
let targetYUpperBound = 97

let rec factorial (n: int) =
    match n with
    | 0 -> 1
    | n -> n + (factorial (n - 1))

let main () =
    let yValues = [targetYLowerBound - 1 .. targetYUpperBound - 1]
    let xValues =
        Seq.initInfinite (fun i -> i, factorial i)
        |> Seq.takeWhile (fun (_, x) -> x <= targetXUpperBound)
//        |> Seq.filter (fun (_, x) -> x >= targetXLowerBound)
//        |> Seq.map fst
        |> Seq.toList
    printfn $"y: %A{yValues}"
    printfn $"x: %A{xValues}"
    List.allPairs xValues yValues
    |> List.length
    |> fun x -> printfn $"Answer: %A{x}"
    
main ()