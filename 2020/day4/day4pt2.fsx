open System.Text.RegularExpressions

type State = {
    valid: int
    current: string
}

let required = [|"byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"|]
let hclRegex = Regex "^#[\da-f]{6}$"
let eclValues = [|"amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|]
let pidRegex = Regex "^\d{9}$"

let validHgt (s: string) =
    let unit = s.Substring (s.Length - 2)
    let number = s.Substring (0, s.Length - 2) |> System.Int32.TryParse
    match unit, number with
    | "cm", (true, x) -> x >= 150 && x <= 193
    | "in", (true, x) -> x >= 59 && x <= 76
    | _ -> false

let checkValue ((k,v): string * string) : bool =
    match k, v with
    | "byr", x -> x.Length = 4 && int x >= 1920 && int x <= 2002
    | "iyr", x -> x.Length = 4 && int x >= 2010 && int x <= 2020
    | "eyr", x -> x.Length = 4 && int x >= 2020 && int x <= 2030
    | "hgt", x -> validHgt x
    | "hcl", x -> hclRegex.IsMatch x
    | "ecl", x -> Array.contains x eclValues
    | "pid", x -> pidRegex.IsMatch x
    | _, _ -> true

let checkValid (entry: string) : int =
    printfn "Entry: %s" entry
    entry.Split ' '
    |> Array.map (fun x -> x.Split ':'|> fun y -> (y.[0], y.[1]))
    |> fun x -> 
        let keys = Array.map fst x
        Array.forall (fun k -> Array.contains k keys) required && Array.forall checkValue x
    |> fun x -> if x then 1 else 0


let fold (state: State) (line: string) =
    match line with
    | "" -> {current = ""; valid = state.valid + (checkValid state.current)}
    | x -> {state with current = (state.current + " " + line).TrimStart()}

let main () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.fold fold {valid = 0; current = ""}
    |> fun x -> x.valid + (checkValid x.current)
    |> printfn "Answer: %d"
    

main ()