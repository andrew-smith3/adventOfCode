
let readFile () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map int
    |> Seq.sortDescending
    |> Seq.toList

let rec canHitTarget (target: int) (numbers: int list) =
    match numbers with 
    | i::rest when List.contains (target - i) rest -> Ok i
    | i::rest -> canHitTarget target rest
    | [] -> Error ()

let rec find (numbers: int list) = 
    match numbers with
    | i::rest ->
        match canHitTarget (2020 - i) rest with
        | Ok n -> (i, n, 2020 - i - n)
        | Error _ -> find rest
    | [] -> failwith "match not found"


let main () = 
    let numbers = readFile ()
    let (x, y, z) = find numbers
    let answer = x * y * z
    printfn "Answer: %d" answer

main ()