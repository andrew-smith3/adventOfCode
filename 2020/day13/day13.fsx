let parse () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.toArray
    |> fun x -> 
        let timestamp = x.[0] |> int
        let buses = x.[1].Split(',') |> Array.filter (fun c -> c <> "x") |> Array.map int
        (timestamp, buses)

let rec findBus (timestamp: int) (buses: int []) (minutes: int) = 
    let time = timestamp + minutes
    let available = Array.filter (fun x -> time % x = 0) buses
    if Array.length available = 1
    then (Array.exactlyOne available) * minutes
    else findBus timestamp buses (minutes + 1)

let main () = 
    parse ()
    |> fun (t, b) -> findBus t b 0
    |> printfn "Answer: %d"


main ()