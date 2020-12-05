
type State = {
    valid: int
    current: string
}

let required = [|"byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"|]

let checkValid (entry: string) : int =
    entry.Split ' '
    |> Array.map (fun x -> x.Split ':'|> fun y -> y.[0])
    |> fun x -> Array.forall (fun k -> Array.contains k x) required
    |> fun x -> if x then 1 else 0


let fold (state: State) (line: string) =
    match line with
    | "" -> {current = ""; valid = state.valid + (checkValid state.current)}
    | x -> {state with current = state.current.Trim() + " " + line}

let main () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.fold fold {valid = 0; current = ""}
    |> fun x -> x.valid + (checkValid x.current)
    |> printfn "Answer: %d"
    

main ()