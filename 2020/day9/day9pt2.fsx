
let target : int64 = 15690279L

let rec hasMatch (values: int64 list) (index: int) (numbers: int64[]) = 
    let values = numbers.[index]::values
    match values with
    | x when List.sum values = target && List.length values >= 2 -> (true, values)
    | x when List.sum values > target -> (false, [])
    | x -> hasMatch x (index + 1) numbers

let rec findSet (index: int) (numbers: int64 []) = 
    match hasMatch [] index numbers with
    | true, x -> (List.min x) + (List.max x)
    | false, _ -> findSet (index+1) numbers

let main () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map int64
    |> Seq.toArray
    |> findSet 0
    |> printfn "Answer: %d"

main ()