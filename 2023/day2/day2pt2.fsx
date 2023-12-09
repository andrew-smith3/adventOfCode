open System

type State = {
    Red: int
    Green: int
    Blue: int
}
with
    static member Init = { Red = 0; Green = 0; Blue = 0 }
    
    member x.Power() = x.Red * x.Green * x.Blue

let folder (state: State) (parts : string array) =
    let num = parts[0] |> Int32.Parse
    match parts[1] with
    | "red" when num > state.Red -> { state with Red = num }
    | "green" when num > state.Green -> { state with Green = num }
    | "blue" when num > state.Blue -> { state with Blue = num }
    | _ -> state

let parseRound (state: State) (s: string) =
    s.Split(", ")
    |> Array.map _.Split(" ")
    |> Array.fold folder state

let parse (sum: int) (s: string) =
    let parts = s.Split(": ")
    let power =
        parts[1].Split("; ")
        |> Array.fold parseRound State.Init
        |> _.Power()
    power + sum
    
let main () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.fold parse 0
    |> fun x -> printfn $"Answer: {x}"
    
main()