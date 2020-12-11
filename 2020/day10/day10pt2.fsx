let increment (toAddKey: int64) (map: Map<int64, int64>) (key: int64)  =
    let toAdd = 
        match Map.tryFind toAddKey map with
        | Some x -> x
        | None -> 1L
    match Map.tryFind key map with
    | Some x -> Map.add key (x + toAdd) map
    | None -> Map.add key toAdd map


let rec arrangements (numbers: int64 list) (values: Map<int64, int64>) = 
    match numbers with
    | number::rest -> 
        List.filter (fun x -> x - number <= 3L) rest
        |> List.fold (increment number) values
        |> arrangements rest
    | [] -> values



let main () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map int64
    |> Seq.append (seq {0L})
    |> Seq.sort
    |> Seq.toList
    |> fun x -> arrangements x Map.empty
    |> Map.toList
    |> List.maxBy snd
    |> snd
    |> printfn "Answer: %d"

main ()