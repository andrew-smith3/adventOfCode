


let parsePair (s: string) =
    s.Split ','
    |> Array.map (fun x ->
        let a = x.Split '-'
        (int a[0], int a[1])
        )
    |> fun x -> (x[0], x[1])
    
let isOverlapping(((f1, s1), (f2, s2)): (int * int) * (int * int)) =
    if f1 >= f2 && f1 <= s2 then 1
    elif s1 >= f2 && s1 <= s2 then 1
    elif f2 >= f1 && f2 <= s1 then 1
    elif s2 >= f1 && s2 <= s1 then 1
    else 0

let accumulate (p : int) (line: string) =
    line
    |> parsePair
    |> isOverlapping
    |> (+) p

let part2 () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.fold accumulate 0
    |> printfn "Answer: %d"
    
part2 ()