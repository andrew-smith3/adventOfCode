open System

type Input = {
    pairs: Map<string, int64>
    map: Map<string, char>
} with
    static member Empty = {pairs = Map.empty; map = Map.empty}
    
let increment (i: int64) (m: Map<string, int64>) (k: string)  =
    match Map.tryFind k m with
    | Some x -> Map.add k (x + i) m
    | None -> Map.add k i m

let readFile () = System.IO.File.ReadLines("input.txt")

let parse (i: Input) (s: string) =
    match s with
    | _ when String.IsNullOrWhiteSpace(s) -> i
    | _ when s.Contains("->") ->
        s.Split(" -> ")
        |> fun x ->
            let m = Map.add x[0] x[1].[0] i.map
            {i with map = m}
    | _ ->
        s
        |> Seq.pairwise
        |> Seq.map (fun (c1, c2) -> new string([|c1; c2|]))
        |> Seq.fold (increment 1L) Map.empty
        |> fun pairs -> {i with pairs = pairs}
    
let fold (map: Map<string, char>) (pairs: Map<string, int64>) (s: string) (i: int64) =
    match Map.tryFind s map with
    | Some x ->
        let pair1 = new string([| s.[0]; x |])
        let pair2 = new string([| x; s.[1] |])
        increment i pairs pair1
        |> fun m -> increment i m pair2
    | None -> increment i pairs s
        
let rec expand (pairs: Map<string, int64>) (map: Map<string, char>) (step: int) =
    match step with
    | 0 -> pairs
    | _ ->
        pairs
        |> Map.fold (fold map) Map.empty
        |> fun m -> expand m map (step - 1)

let count (pairs: Map<string, int64>) =
    let counts = 
        pairs
        |> Map.toArray
        |> Array.collect (fun (s, i) -> [|(s[0], i); (s[1], i)|])
        |> Array.groupBy fst
        |> Array.map (fun (k, v) -> (k, v |> Array.map snd |> Array.sum))
        |> Array.map (snd >> (fun y -> Math.Ceiling(float y / float 2)) >> int64)
        |> fun x ->
            printfn $"counts: %A{x}"
            x 
    let max = counts |> Array.max
    let min = counts |> Array.min
    max - min

let main () =
    readFile ()
    |> Seq.fold parse Input.Empty
    |> fun i ->
        printfn $"i: %A{i}"
        expand i.pairs i.map 40
    |> count
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()