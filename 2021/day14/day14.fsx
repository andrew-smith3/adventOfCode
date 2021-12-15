open System

type Input = {
    template: string
    map: Map<string, string>
} with
    static member Empty = {template = ""; map = Map.empty}

let readFile () = System.IO.File.ReadLines("test.txt")

let parse (i: Input) (s: string) =
    match s with
    | _ when String.IsNullOrWhiteSpace(s) -> i
    | _ when s.Contains("->") ->
        s.Split(" -> ")
        |> fun x ->
            let m = Map.add x[0] x[1] i.map
            {i with map = m}
    | _ ->
        {i with template = s}
    
let fold (map: Map<string, string>) (s: string) ((c1, c2): char * char) =
    let k = new string([|c1; c2|])
    match Map.tryFind k map with
    | Some x -> s + string c1 + x
    | None -> s + string c1
        
let rec expand (template: string) (map: Map<string, string>) (step: int) =
    match step with
    | 0 -> template
    | _ ->
        let lastChar = template[template.Length - 1]
        template
        |> Seq.pairwise
        |> Seq.fold (fold map) ""
        |> fun s -> s + string lastChar
        |> fun s -> expand s map (step - 1)

let count (s: string) =
    let counts = 
        s
        |> Seq.countBy id
        |> Seq.toArray
    let max = counts |> Array.maxBy snd |> snd
    let min = counts |> Array.minBy snd |> snd
    max - min

let main () =
    readFile ()
    |> Seq.fold parse Input.Empty
    |> fun i -> expand i.template i.map 40
    |> count
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()