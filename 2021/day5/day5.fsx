let readFile () = System.IO.File.ReadLines("input.txt")

let parseCoord (s: string) =
    let coord = s.Split ','
    (int coord.[0], int coord.[1])
   
let parse (lines: seq<string>) =
    lines
    |> Seq.map (fun x -> x.Split " -> ")
    |> Seq.map (fun x ->
        let pointA = parseCoord x.[0]
        let pointB = parseCoord x.[1]
        pointA, pointB
        )

let coordsToPoints (c:(int * int) * (int * int)) =
    match c with
    | (x1, y1), (x2, y2) when x1 = x2 ->
        let max = System.Math.Max(y1, y2)
        let min = System.Math.Min(y1, y2)
        seq {for i in min .. max -> (x1, i)}
    | (x1, y1), (x2, y2) when y1 = y2 ->
        let max = System.Math.Max(x1, x2)
        let min = System.Math.Min(x1, x2)
        seq {for i in min .. max -> (i, y1)}
    | _ -> failwith "Error"

let find (coords: seq<(int * int) * (int * int)>) =
    coords
    |> Seq.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)
    |> Seq.collect coordsToPoints
    |> Seq.countBy id
    |> Seq.filter (snd >> (fun x -> x > 1))
    |> Seq.length
    
let main () =
    readFile ()
    |> parse
    |> find
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()