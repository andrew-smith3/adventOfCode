
let foldRow (v: int) (c: char) =
    match c with
    | 'F' -> v <<< 1
    | 'B' -> (v <<< 1) ||| 1
    | _ -> v

let foldCol (v: int) (c: char) =
    match c with
    | 'L' -> v <<< 1
    | 'R' -> (v <<< 1) ||| 1
    | _ -> v

let parse (s: string) : int =
    let row = s.Substring(0, 7) |> Seq.fold foldRow 0
    let col = s.Substring(7) |> Seq.fold foldCol 0
    row * 8 + col    

let main () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map parse
    |> Seq.sort
    |> Seq.pairwise
    |> Seq.find (fun (x, y) -> y - x = 2)
    |> fun (x, y) -> x + 1
    |> printfn "Answer: %d"

    

main ()