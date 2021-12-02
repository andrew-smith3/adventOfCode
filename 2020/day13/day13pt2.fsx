let parse () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.toArray
    |> fun x -> 
        x.[1].Split(',') 
        |> Array.mapi (fun i c -> (c, int64 i)) 
        |> Array.filter (fun (c, i) -> c <> "x")
        |> Array.map (fun (c, i) -> int64 c, i)

let rec findStart (timestamp: int64) ((bus, delay): int64 * int64) = 
    if (timestamp + delay) % bus = 0L
    then timestamp
    else findStart (timestamp + 1L) (bus, delay)

let rec findSec (timestamp: int64) (interval: int64) (buses: (int64 * int64) []) = 
    if Array.forall (fun (c, i) -> (timestamp + i) % c = 0L) buses
    then timestamp
    else findSec (timestamp + interval) interval buses

let main () = 
    parse ()
    |> fun buses -> 
        printfn "%A" buses
        let firstBus = Array.maxBy fst buses
        let start = findStart 1000000000000L firstBus
        (start, firstBus, buses)
    |> fun (start, firstBus, buses) -> findSec start (fst firstBus) buses
    |> printfn "Answer: %d"


main ()