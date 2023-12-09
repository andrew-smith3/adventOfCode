open System

let getNumber (s: string) =
    s
    |> Seq.filter Char.IsDigit
    |> Seq.toArray
    |> fun x ->
        let first = x[0]
        let last = x[x.Length - 1]
        Int32.Parse($"{first}{last}")


let part1 () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map getNumber
    |> Array.sum
    |> fun x -> printfn $"Answer: {x}"

let substring (s: string) (pos: int) (n: int) =
    try
        s.Substring(pos, n)
    with
    | _ -> ""

let rec findNumber (s: string) (pos: int) (modifier: int) =
    match s[pos] with
    | x when Char.IsDigit(x) -> (int (string x))
    | 'o' when substring s pos 3 = "one" -> 1
    | 't' when substring s pos 3 = "two" -> 2
    | 't' when substring s pos 5 = "three" -> 3
    | 'f' when substring s pos 4 = "four" -> 4
    | 'f' when substring s pos 4 = "five" -> 5
    | 's' when substring s pos 3 = "six" -> 6
    | 's' when substring s pos 5 = "seven" -> 7
    | 'e' when substring s pos 5 = "eight" -> 8
    | 'n' when substring s pos 4 = "nine" -> 9
    | _ -> findNumber s (pos + modifier) modifier 

let parseNumbers (s: string) =
    let first = findNumber s 0 1
    let last = findNumber s (s.Length - 1) -1
    Int32.Parse($"{first}{last}")

let part2 () =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map parseNumbers
    |> Array.sum
    |> fun x -> printfn $"Answer 2: {x}"
    
part1()    
part2()