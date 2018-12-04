open System;

let test = "+3\n+3\n+4\n-2\n-4"

type Change =  
    | Positive
    | Negative

type FrequencyChange = {
    op: Change;
    amount: int;
}

type State = {
    found: int option;
    frequency: int;
    prevFreqs: Set<int>
}

let stringToFrequencyChange (s: string) : FrequencyChange = 
    let sign = s.Chars(0)
    let number = s.Substring(1) |> Int32.Parse
    match sign with
    | '+' -> {op=Positive; amount=number}
    | '-' -> {op=Negative; amount=number}
    | _ -> failwith "Invalid sign"

let readInputFile () = 
    System.IO.File.ReadAllLines("./2018/day1/input.txt") 
    |> Array.filter (fun s -> s.Length > 0)
    |> Array.map stringToFrequencyChange
    |> Array.toList

let reduceFreq (state: int) (fc: FrequencyChange) : int = 
    match fc.op with
    | Positive -> state + fc.amount
    | Negative -> state - fc.amount

let rec helper (state: State) (fcs: FrequencyChange list) : State =
    match fcs with
    | [] -> state
    | fc::rest -> 
        let freq = reduceFreq state.frequency fc
        if Set.contains freq state.prevFreqs 
        then {state with found = Some freq}
        else 
            let set = Set.add freq state.prevFreqs
            let newState = {state with frequency = freq; prevFreqs=set}
            helper newState rest

let findRepeater (fcs: FrequencyChange list) : int = 
    let rec inner (state: State) = 
        let result = helper state fcs
        match result.found with
        | Some x -> x
        | None -> inner result
    inner {found=None; frequency=0; prevFreqs=Set.empty}

let main () = 
    readInputFile()
    |> findRepeater
    |> printfn "Answer is: %i"