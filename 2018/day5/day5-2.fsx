open System;

let readInputFile () = System.IO.File.ReadAllText("./2018/day5/input.txt") 

let charsCancel (a: char) (b: char) = a <> b && (Char.ToLower a) = (Char.ToLower b)

let folder (state: char list) (c: char) : char list = 
    if List.isEmpty state 
    then c::state 
    else
        match (charsCancel c (List.head state)) with
        | true -> List.tail state
        | false -> c::state

let processUnits (s: string) = 
    Seq.fold folder [] s 
    |> List.length

let stripChars (c: char) (s: string) = 
    let upper = Char.ToUpper c |> Char.ToString
    let lower = Char.ToLower c |> Char.ToString
    s.Replace(upper, "").Replace(lower, "")

let findBestReaction (s: string) = 
    let folder (map: Map<char, int>) (c: char) : Map<char, int> =
        let key = Char.ToLower c
        match Map.tryFind key map with
        | Some _ -> map
        | None ->  
            let count = stripChars key s |> processUnits
            Map.add key count map
    Seq.fold folder Map.empty s
    |> Map.toList
    |> List.minBy snd
    |> snd

let main() = 
    readInputFile()
    |> findBestReaction
    |> printfn "%i" 