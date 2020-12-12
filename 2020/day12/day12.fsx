open System

type State = {
    x: int
    y: int
    direction: char
}

let directions = [|'N'; 'E'; 'S'; 'W'; |]

let parse () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map (fun x -> Seq.head x, int (Seq.tail x |> String.Concat))

let getDirection (current: char) (turn: int) = 
    Array.findIndex (fun x -> x = current) directions 
        |> fun x -> ((x + (turn / 90)) % 4 + 4) % 4
        |> fun x -> directions.[x]

let rec move (state: State) ((c, i): char * int) = 
    match c with
    | 'N' -> {state with y = state.y + i}
    | 'S' -> {state with y = state.y - i}
    | 'E' -> {state with x = state.x + i}
    | 'W' -> {state with x = state.x - i}
    | 'F' -> move state (state.direction, i)
    | 'R' -> {state with direction = (getDirection state.direction i)}
    | 'L' -> {state with direction = (getDirection state.direction (i * -1))}
    | _ -> failwithf "Unrecognized command: %c" c

let main () = 
    parse ()
    |> Seq.fold move {x = 0; y = 0; direction = 'E'}
    |> fun state -> Math.Abs(state.x) + Math.Abs(state.y)
    |> printfn "Answer: %d"

main ()
    