
let readInput () = 
    System.IO.File.ReadLines("input.txt")

let traverse ((col, count): int * int) (line: string) : int * int = 
    let index = if col > line.Length then col % line.Length else col
    let count = if line.[index] = '#' then count + 1 else count
    (col+3, count)

let main () = 
    readInput ()
    |> Seq.fold traverse (0, 0)
    |> fun (_, count) -> printfn "Answer: %d" count


main ()