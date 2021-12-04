open System

let readFile () = 
    System.IO.File.ReadLines("input.txt")

type Mode =
    | Max
    | Min

let tiebreak (mode: Mode) = if mode = Max then 1 else 0

let f (mode: Mode) = if mode = Max then Array.maxBy else Array.minBy

let filter (lines: int[][]) (pos: int) (mode: Mode) : int[][] =
    let keep =
        lines
        |> Array.map (fun x -> x.[pos])
        |> Array.countBy id
        |> fun x ->
            let isTie =
                x
                |> Array.distinctBy snd
                |> Array.length
                |> fun l -> l = 1
            if isTie then tiebreak mode else ((f mode) snd x) |> fst
    Array.filter (fun x -> x.[pos] = keep) lines

let rec find (lines: int[][]) (pos: int) (mode: Mode) =
    match lines with
    | [||] -> failwith "Error"
    | [|x|] ->
        Array.fold (fun s a -> (s <<< 1) ||| a) 0 x
    | _ ->
        let filteredLines = filter lines pos mode
        find filteredLines (pos + 1) mode
    

let calc (lines: string[]) =
    let l =
        lines
        |> Array.map (Seq.map (int >> (+) -48) >> Seq.toArray)
    let oxygen = find l 0 Max
    let co2 = find l 0 Min
    oxygen * co2    
    
let main () =
    readFile ()
    |> Seq.toArray
    |> calc
    |> fun x -> printfn $"Answer: %d{x}"

main ()
