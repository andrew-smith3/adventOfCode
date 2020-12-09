
let parseInstruction (i: string) = 
    i.Split(' ')
    |> fun x -> (x.[0], int (x.[1]))

let rec execute (instructions: string[]) (past: int list) (acc: int) (ptr: int) = 
    if List.contains ptr past 
    then acc
    else
        let past = ptr::past
        match parseInstruction instructions.[ptr] with
        | "acc", x -> execute instructions past (acc + x) (ptr + 1)
        | "jmp", x -> execute instructions past acc (ptr + x)
        | "nop", _ -> execute instructions past acc (ptr + 1)
        | _, _ -> failwith "Unsupported match"
    

let main () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.toArray
    |> fun x -> execute x [] 0 0
    |> printfn "Answer: %d"


main ()