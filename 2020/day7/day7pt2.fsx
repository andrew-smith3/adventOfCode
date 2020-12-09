open System
open System.Text.RegularExpressions

let replaceRegex = Regex "\d|bags?|\."

let parseBag (s: string) = replaceRegex.Replace(s, "").Trim()

let parseBagWithAmount (s: string) : int * string = 
    let color = replaceRegex.Replace(s, "").Trim()
    if color = "no other" then (0, "")
    else
    let amount = s.TrimStart().Split().[0] |> int
    (amount, color)


let parseChildren (s: string) = 
    s.Split(',')
    |> Array.map parseBagWithAmount
    |> Array.where (fun (x, _) -> x <> 0)

let populate (key: string) (m: Map<string, (int * string) list>) (value: int * string) = 
    match m.TryFind key with
    | Some v -> 
        m.Add (key, value::v)
    | None -> 
        m.Add (key, [value])

let parseLine (map: Map<string, (int * string) list>) (s: string) = 
    s.Split([|"contain"|], StringSplitOptions.None)
    |> fun x -> 
        match x with
        | [|first; second|] -> 
            let bag = parseBag first
            let children = parseChildren second
            Array.fold (populate bag) map children
        | _ -> failwith (sprintf "Can't parse line: %s" s)

let rec traverse (map: Map<string, (int * string) list>) (color: string) : int =
    match Map.tryFind color map with
    | Some x ->
        List.sumBy (fun (a, c) -> a + a * (traverse map c)) x
    | None -> 
        0


let main () =
    System.IO.File.ReadLines("input.txt")
    |> Seq.fold parseLine Map.empty 
    |> fun m -> traverse m "shiny gold"
    |> printfn "Answer: %d"

main ()