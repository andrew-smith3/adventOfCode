open System


let calculate (i: int) (s: string) =
    s
    |> _.Split(": ")
    |> fun x -> x[1]
    |> _.Split(" | ")
    |> Array.map (_.Split(" ", (StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)))
    |> fun x ->
        x[0]
        |> Array.filter (fun n -> x[1] |> Array.contains n)
        |> Array.length
    |> fun x -> (i + 1, x)
    
let increment (original: int) (map: Map<int, int>) (i: int) =
    let numCopies = 
        match Map.tryFind original map with
            | Some x -> x
            | None -> 1
    let value =
        match Map.tryFind i map with
        | Some x -> x
        | None -> 1
    
    map.Add(i, (value + numCopies))

let folder (map: Map<int, int>) ((i, x): int * int) =
    match x with
    | 0 when not (Map.containsKey i map) -> map.Add(i, 1)
    | 0 -> map
    | _ -> [i + 1 .. i + x] |> List.fold (increment i) map
    

let main () =
    let map = Map.empty |> Map.add 1 1
    System.IO.File.ReadAllLines("input.txt")
    |> Array.mapi calculate
    |> Array.fold folder map
    |> Map.values
    |> Seq.sum
    |> fun x -> printfn $"Answer: {x}"
    
    
    
main()