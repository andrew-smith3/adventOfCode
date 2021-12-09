let segmentCounts = [2; 3; 4; 7;]

let readFile () = System.IO.File.ReadLines("input.txt")

let count (lines: seq<string>) =
    lines
    |> Seq.map ((fun x -> x.Split " | ") >> (fun y -> y.[1]))
    |> Seq.collect (fun x -> x.Split " ")
    |> Seq.filter (fun x -> List.contains x.Length segmentCounts)
    |> Seq.length

let main () =
    readFile ()
    |> count
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()