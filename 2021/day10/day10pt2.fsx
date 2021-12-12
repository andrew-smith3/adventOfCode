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
    
let closingChar (c: char) =
    match c with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> failwith "Error"

let points (c: char) =
    match c with
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L
    | _ -> 0L

let scoreClosing (chars: char list) =
    chars
    |> List.fold (fun s x -> s * 5L + (points x) ) 0L

let rec score (stack: char list) (line: char list) =
    match line with
    | [] ->
        stack
        |> List.map closingChar
        |> scoreClosing
    | h::tail when isStartingChar h ->
        score (h::stack) tail
    | h::tail when h = closingChar (List.head stack)->
        score (List.tail stack) tail
    | _ -> -1L
       

let main () =
    readFile ()
    |> Seq.map (score [])
    |> Seq.filter (fun x -> x <> -1L)
    |> Seq.sort
    |> Seq.toArray
    |> fun x ->
        let index = (Array.length x) / 2
        printfn $"Answer: %d{x.[index]}"
    
main ()