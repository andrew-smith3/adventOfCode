open System;
let testData = "+1\n+1\n+1"

type Change =  
    | Positive
    | Negative

type FrequencyChange = {
    op: Change;
    amount: int;
}

let stringToFrequencyChange (s: string) : FrequencyChange = 
    let sign = s.Chars(0)
    let number = s.Substring(1) |> Int32.Parse
    match sign with
    | '+' -> {op=Positive; amount=number}
    | '-' -> {op=Negative; amount=number}
    | _ -> failwith "Invalid sign"

let reduceFreq (state: int) (fc: FrequencyChange) : int = 
    match fc.op with
    | Positive -> state + fc.amount
    | Negative -> state - fc.amount

let parseInput (input: string) : int =
    input.Split [|'\n'|]
    |> Array.filter (fun s -> s.Length > 0)
    |> Array.map stringToFrequencyChange
    |> Array.fold reduceFreq 0

let main (input: string) = 
    input
    |> parseInput
    |> printfn "Answer is: %i"