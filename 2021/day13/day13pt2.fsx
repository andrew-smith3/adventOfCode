open System;

type Input = {
    dots: (int * int) list
    folds: (string * int) list
} with
    static member Empty = {dots = []; folds = []}

let readFile () = System.IO.File.ReadLines("input.txt")

let parse (i: Input) (s: string) =
    match s with
    | _ when String.IsNullOrWhiteSpace(s) -> i
    | _ when Char.IsDigit(s, 0) ->
        s.Split(',')
        |> Array.map int
        |> fun x ->
            {i with dots = (x.[0], x.[1])::i.dots}
    | _ ->
        s.Replace("fold along ", "")
        |> fun x -> x.Split("=")
        |> fun x ->
            let n = x.[1] |> int
            {i with folds = (x.[0], n)::i.folds}

let fold (dots: (int * int) list) ((direction, n): (string * int)) =
    match direction with
    | "x" ->
        dots
        |> List.map (fun (x, y) ->
            if x > n
            then
                let diff = x - n
                let newX = n - diff
                (newX, y)
            else (x, y)
            )
        |> List.distinct
    | "y" ->
        dots
        |> List.map (fun (x, y) ->
            if y > n
            then
                let diff = y - n
                let newY = n - diff
                (x, newY)
            else (x, y)
            )
        |> List.distinct
    | _ -> failwith "Error"
    
let dotsToArray (dots: (int * int) list) =
    let maxX = dots |> List.map fst |> List.max
    let maxY = dots |> List.map snd |> List.max
    let array: string[,] = Array2D.zeroCreate (maxX + 1) (maxY+1) |> Array2D.map (fun _ -> ".")
    dots
    |> List.iter (fun (x, y) -> array.[x,y] <- "#")
    array


let main () =
    readFile ()
    |> Seq.fold parse Input.Empty
    |> fun i ->
        i.folds
        |> List.rev
        |> List.fold fold i.dots
    |> dotsToArray
    |> fun x ->
        printfn "Answer:"
        printfn $"%A{x}"
    
main ()