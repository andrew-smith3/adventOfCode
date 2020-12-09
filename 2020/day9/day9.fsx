
let preambleLength = 25

let rec hasMatch (number: int64) (numbers: int64[]) = 
    match Array.head numbers, Array.tail numbers with
    | x, y when Array.contains (number - x) y && number - x <> x -> true
    | x, [||] -> false
    | x, y -> hasMatch number y

let rec findNumber (index: int) (numbers: int64 []) = 
    let prev = Array.sub numbers (index - preambleLength) preambleLength
    let number = numbers.[index]
    match hasMatch number prev with
    | false -> number
    | true -> findNumber (index+1) numbers

let main () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map int64
    |> Seq.toArray
    |> findNumber preambleLength
    |> printfn "Answer: %d"

main ()