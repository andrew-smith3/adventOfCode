let readFile () =
    System.IO.File.ReadLines("input.txt")
    |> Seq.map (Seq.toList)

let isStartingChar (c: char) =
    match c with
    | '(' -> true
    | '[' -> true
    | '{' -> true
    | '<' -> true
    | _ -> false
    
let isMatch (start: char) (c: char) =
    match (start, c) with
    | ('(', ')') -> true
    | ('[', ']') -> true
    | ('{', '}') -> true
    | ('<', '>') -> true
    | _ -> false

let points (c: char) =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let rec score (stack: char list) (line: char list) =
    match line with
    | [] -> 0
    | h::tail when isStartingChar h ->
        score (h::stack) tail
    | h::tail when (isMatch (List.head stack) h)->
        score (List.tail stack) tail
    | h::_ -> points h

let main () =
    readFile ()
    |> Seq.map (score [])
    |> Seq.sum
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()