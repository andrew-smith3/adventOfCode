open System

type Board = (int * bool)[][]

let readFile () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.toArray
    
let parse (lines: string[]) : int list * Board[] =
    let numbers =
        lines.[0]
        |> fun x -> x.Split ','
        |> Array.map int
        |> Array.toList
    let boards =
        Array.tail lines
        |> Array.filter (String.IsNullOrWhiteSpace >> not)
        |> Array.map (fun x -> x.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun y -> (int y, false)))
        |> Array.chunkBySize 5
    
    numbers, boards
    
let markBoards (boards: Board[]) (n: int) : Board[] =
    boards
    |> Array.map (Array.map (Array.map (fun (x, m) -> if x = n then (x, true) else (x, m) )))
    
let checkHorizontalWin = Array.exists (Array.forall (snd >> (=) true))    
    
let checkWin (boards: Board[]) : Board option =
    let checkBoard (board: Board) =
        let horizontalWin = board |> checkHorizontalWin
        let verticalWin = board |> Array.transpose |> checkHorizontalWin
        horizontalWin || verticalWin
        
    boards
    |> Array.tryFind checkBoard

let rec bingo ((numbers, boards) : int list * Board[]) =
    match numbers with
    | h::t ->
        let boards = markBoards boards h
        match checkWin boards with
        | None -> bingo (t, boards)
        | Some b -> b, h
    | _ -> failwith "Error"
    
let calcAnswer ((board, n): Board * int) =
    board
    |> Array.collect id
    |> Array.filter (fun (_, m) -> not m)
    |> Array.sumBy fst
    |> fun s -> s * n
    

let main () =
    readFile ()
    |> parse
    |> bingo
    |> calcAnswer
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()