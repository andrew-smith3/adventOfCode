
let test = Map [(0,1); (3,2); (6,3)]

let input = Map [(16,1); (1,2); (0,3); (18,4); (12, 5); (14, 6); (19, 7)]

let unfold ((map, (lastCalled, lastTurn)): Map<int,int> * (int * int) ) =
    let currentTurn = lastTurn + 1
    let numberToCall =
        match Map.tryFind lastCalled map with
        | Some previousCalledTurn ->
            lastTurn - previousCalledTurn
        | None -> 0
    let map = Map.add lastCalled lastTurn map
    let next = numberToCall, currentTurn
    Some (next, (map, next))
    
let main (map: Map<int, int>) (turnToFind: int) =
    let last = Map.toList map |> List.maxBy snd
    Seq.unfold unfold (map, last)
    |> Seq.find (fun (_,t) -> t = turnToFind)
    |> fst
    |> fun x -> printfn $"Answer: %d{x}"
    
main input 30000000
