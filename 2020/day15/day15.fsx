
let test = [(0,1); (3,2); (6,3)] |> List.rev

let input = [(16,1); (1,2); (0,3); (18,4); (12, 5); (14, 6); (19, 7)] |> List.rev

let unfold ((list, currentTurn): (int * int) list * int) =
    let (h, turnBefore)::tail = list
    match List.tryFind (fun (x, _) -> x = h) tail with
    | Some (_, lastTurnCalled) ->
        let numberToCall = turnBefore - lastTurnCalled
        Some((numberToCall, currentTurn), ((numberToCall, currentTurn)::list, currentTurn+1))
    | None -> Some ((0, currentTurn), ((0, currentTurn)::list, currentTurn+1))
    

let main startList turnToFind =
    let nextTurn = startList |> List.length |> (+) 1 
    Seq.unfold unfold (startList, nextTurn)
    |> Seq.find (fun (_,t) -> t = turnToFind)
    |> fst
    |> fun x -> printfn $"Answer: %d{x}"
    
main input 2020

