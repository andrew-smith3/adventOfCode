let readFile () =
    System.IO.File.ReadLines("input.txt")
    |> Seq.toArray
    |> fun x -> x.[0].Split ','
    |> Array.map int
    |> Array.countBy id
    |> Map.ofArray
    |> Map.map (fun _ v -> int64 v)

let processDay (map: Map<int, int64>) =
    map
    |> Map.fold (fun m key value ->
        let newKey, newFish = if key - 1 < 0 then 6, value else key - 1, 0
        let existing =
            match Map.tryFind newKey m with
            | Some x -> x
            | None -> 0L
        m
        |> Map.add newKey (existing + value)
        |> Map.change 8 (fun x ->
            match x with
            | Some n -> Some (n + newFish)
            | None -> Some newFish
            )
        ) Map.empty

let rec passDay (target: int) (current: int) (map: Map<int, int64>) =
    if current = target
    then map
    else passDay target (current + 1) (processDay map)
    
let totalFish (map: Map<int, int64>) =
    map
    |> Map.toSeq
    |> Seq.sumBy snd
    
let main () =
    readFile ()
    |> passDay 256 0
    |> totalFish
    |> fun x -> printfn $"Answer: %d{x}"
    
    
main ()