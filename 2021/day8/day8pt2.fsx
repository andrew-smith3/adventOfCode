open System

let readFile () = System.IO.File.ReadLines("input.txt")

let findUniqueDigit (s: string) =
    match s with
    | _ when s.Length = 2 -> Some 1
    | _ when s.Length = 3 -> Some 7
    | _ when s.Length = 4 -> Some 4
    | _ when s.Length = 7 -> Some 8
    | _ -> None
    
let sortStringChars (s: string) =
    s
    |> Seq.sort
    |> String.Concat
    
let decode ((key, value): string * string) =
    let uniqueNumbers, unknown =
        key.Split " "
        |> Array.map (fun x -> (sortStringChars x), (findUniqueDigit x))
        |> Array.partition (snd >> Option.isSome)
    let keyMap =
        uniqueNumbers
        |> Array.map (fun (k, v) -> k, Option.get v)
        |> Map.ofArray
    let sixCharacterNumbers = unknown |> Array.map fst |> Array.filter (fun s -> s.Length = 6)
    let fiveCharacterNumbers = unknown |> Array.map fst |> Array.filter (fun s -> s.Length = 5)
    let one = Map.findKey (fun _ v -> v = 1) keyMap
    let six = sixCharacterNumbers |> Array.find (fun s -> Set.intersect (set s) (set one) |> Set.count |> fun x -> x = 1)
    let three = fiveCharacterNumbers |> Array.find (fun s -> Set.intersect (set s) (set one) |> Set.count |> fun x -> x = 2)
    let nine = sixCharacterNumbers |> Array.find (fun s -> Set.intersect (set s) (set three) |> Set.count |> fun x -> x = 5)
    let zero = sixCharacterNumbers |> Array.find (fun s -> s <> six && s <> nine) |> sortStringChars
    let five = fiveCharacterNumbers |> Array.find (fun s -> Set.intersect (set s) (set six) |> Set.count |> fun x -> x = 5)
    let two = fiveCharacterNumbers |> Array.find (fun s -> s <> three && s <> five) |> sortStringChars
    let keyMap = 
        keyMap
        |> Map.add six 6
        |> Map.add three 3
        |> Map.add nine 9
        |> Map.add zero 0
        |> Map.add five 5
        |> Map.add two 2
    
    value.Split " "
    |> Array.map ((fun x -> Map.find (sortStringChars x) keyMap) >> string)
    |> String.concat ""
    |> int
    
let parse (lines: seq<string>) =
    lines
    |> Seq.map ((fun x -> x.Split " | ") >> (fun x -> x.[0], x.[1]))
    
let main () =
    readFile ()
    |> parse
    |> Seq.map decode
    |> Seq.sum
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()