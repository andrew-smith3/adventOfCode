
let readInput () = 
    System.IO.File.ReadLines("input.txt")

let traverse step ((col, count): int * int) (line: string) : int * int = 
    let index = if col >= line.Length then col % line.Length else col
    let count = if line.[index] = '#' then count + 1 else count
    (col+step, count)

let count (input: string list) step =
    input
    |> List.fold (traverse step) (0,0)
    |> snd


let main () = 
    let input = readInput () |> Seq.toList
    let x = 
        [1;3;5;7]
        |> List.map (count input)
        |> List.reduce (*)
        |> int64
    let y = 
        input
        |> List.mapi (fun i el -> el, i)
        |> List.filter (fun (el, i) -> i % 2 = 0)
        |> List.map fst
        |> fun i -> count i 1
        |> int64
    printfn "Answer: %d" (x * y)


main ()