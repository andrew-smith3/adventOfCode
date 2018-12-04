type State = {
    twos: int
    threes: int
}

let readInputFile () = 
    System.IO.File.ReadAllLines("./2018/day2/input.txt") 
    |> Array.toList

let countLetters (s: string) = 
    s 
    |> Seq.countBy (id)
    |> (fun x -> printfn "%A" x; x)
    |> Seq.filter (fun (c, i) -> i = 2 || i = 3)
    |> Seq.map (fun (c, i) -> i)
    |> Seq.distinct
    |> Seq.sum

let folder (state: State) (input: string) : State = 
    match countLetters input with
    | 5 -> {state with twos=state.twos+1; threes=state.threes+1}
    | 3 -> {state with threes=state.threes+1}
    | 2 -> {state with twos=state.twos+1}
    | _ -> state

let stateToChecksum (s: State) = s.twos * s.threes

let main () = 
    readInputFile()
    |> List.fold folder {twos=0; threes=0}
    |> stateToChecksum