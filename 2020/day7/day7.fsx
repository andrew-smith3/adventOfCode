open System
open System.Text.RegularExpressions

let replaceRegex = Regex "\d|bags?|\."

let parseBag (s: string) = replaceRegex.Replace(s, "").Trim()

let parseChildren (s: string) = 
    s.Split(',')
    |> Array.map parseBag
    |> Array.where (fun x -> x <> "no other")

let populate (value: string) (m: Map<string, string list>)  (key: string) = 
    match m.TryFind key with
    | Some v -> 
        m.Add (key, value::v)
    | None -> 
        m.Add (key, [value])

let parseLine (map: Map<string, string list>) (s: string) = 
    s.Split([|"contain"|], StringSplitOptions.None)
    |> fun x -> 
        match x with
        | [|first; second|] -> 
            let bag = parseBag first
            let children = parseChildren second
            Array.fold (populate bag) map children
        | _ -> failwith (sprintf "Can't parse line: %s" s)

let rec traverse (map: Map<string, string list>) (key: string) : string list = 
    match Map.tryFind key map with
    | Some x ->
        List.collect (traverse map) x 
        |> List.append x
        |> List.distinct
    | None -> 
        []



let main () =
    System.IO.File.ReadLines("input.txt")
    |> Seq.fold parseLine Map.empty 
    |> fun m -> traverse m "shiny gold"
    |> List.length
    |> printfn "Answer: %d"

main ()