open System

type State = {
    shipX: int
    shipY: int
    wayX: int
    wayY: int
}

let parse () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map (fun x -> Seq.head x, int (Seq.tail x |> String.Concat))

let rotate (state: State) (amount: int) =
    let rotation = ((amount / 90) % 4 + 4) % 4
    match rotation with
    | 1 -> {state with wayX = state.wayY; wayY = state.wayX * -1}
    | 2 -> {state with wayX = state.wayX * -1; wayY = state.wayY * -1}
    | 3 -> {state with wayX = state.wayY * -1; wayY = state.wayX}
    | _ -> state

let rec move (state: State) ((c, i): char * int) = 
    match c with
    | 'N' -> {state with wayY = state.wayY + i}
    | 'S' -> {state with wayY = state.wayY - i}
    | 'E' -> {state with wayX = state.wayX + i}
    | 'W' -> {state with wayX = state.wayX - i}
    | 'F' -> {state with shipX = state.shipX + (i * state.wayX); shipY = state.shipY + (i * state.wayY)}
    | 'R' -> rotate state i
    | 'L' -> rotate state (i * -1)
    | _ -> failwithf "Unrecognized command: %c" c

let main () = 
    parse ()
    |> Seq.fold move {shipX = 0; shipY = 0; wayX = 10; wayY = 1}
    |> fun state -> Math.Abs(state.shipX) + Math.Abs(state.shipY)
    |> printfn "Answer: %d"

main ()
    