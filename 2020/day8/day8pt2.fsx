
let parseInstruction (i: string) = 
    i.Split(' ')
    |> fun x -> (x.[0], int (x.[1]))

let rec execute (swap: int list) (swapped: bool) (instructions: string[]) (past: int list) (acc: int) (ptr: int) : Result<int, int list> = 
    if List.contains ptr past 
    then Error swap
    else
        let past = ptr::past
        if ptr = Array.length instructions 
        then Ok acc 
        else
            match parseInstruction instructions.[ptr] with
            | "acc", x -> execute swap swapped  instructions past (acc + x) (ptr + 1)
            | "jmp", x -> 
                if swapped || List.contains ptr swap
                then execute swap swapped instructions past acc (ptr + x)
                else 
                    let swap = ptr::swap
                    execute swap true instructions past acc (ptr + 1)
            | "nop", x -> 
                if swapped || List.contains ptr swap
                then execute swap swapped instructions past acc (ptr + 1)
                else
                    let swap = ptr::swap
                    execute swap swapped instructions past acc (ptr + x)
            | _, _ -> failwith "Unsupported match"
 
let rec iterate (instructions: string[]) (swap: int list) = 
    let result = execute swap false instructions [] 0 0
    match result with
    | Ok x -> x
    | Error swap -> iterate instructions swap

let main () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.toArray
    |> fun x -> iterate x []
    |> printfn "Answer: %d"


main ()