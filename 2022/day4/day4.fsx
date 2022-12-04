


let parsePair (s: string) =
    s.Split ','
    |> Array.map (fun x ->
        let a = x.Split '-'
        (int a[0], int a[1])
        )
    |> fun x -> (x[0], x[1])
    
let isContained(((f1, s1), (f2, s2)): (int * int) * (int * int)) =
    if f1 <= f2 && s1 >= s2
    then 1
    elif f2 <= f1 && s2 >= s1
    then 1
    else 0

let accumulate (p : int) (line: string) =
    line
    |> parsePair
    |> isContained
    |> (+) p

let part1 () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.fold accumulate 0
    |> printfn "Answer: %d"
    
part1 ()