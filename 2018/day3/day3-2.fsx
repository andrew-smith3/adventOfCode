type Claim = {
    id: int;
    top: int;
    left: int;
    width: int;
    height: int;
}

let readInputFile () = 
    System.IO.File.ReadAllLines("./2018/day3/input.txt") 
    |> Array.toList

let parseClaim (s:string) : Claim =
    let sections = s.Split([|' '|])
    match sections with
    | [|i; _; coord; dimensions;|] -> 
        let id = i.Replace("#", "") |> int
        let c = coord.Replace(":", "").Split([|','|])
        let left = Array.item 0 c |> int
        let top =  Array.item 1 c |> int
        let d = dimensions.Split([|'x'|])
        let width = Array.item 0 d |> int
        let height = Array.item 1 d |> int
        {id=id; left=left; top=top; width=width; height=height;}
    | _ -> failwith "Error"

let claimToIdCoords (claim: Claim) : (int * (int * int)) list = 
    let lastX = claim.left + claim.width - 1
    let lastY = claim.top + claim.height - 1
    [for i in claim.left .. lastX do
        for j in claim.top .. lastY -> (claim.id, (i, j))]

let findSingleArea (idCoords: (int * (int * int)) list) : int = 
    let coordCounts = idCoords |> List.map snd |> List.countBy id |> Map.ofList
    idCoords 
    |> List.groupBy fst 
    |> List.map (fun (id, coords) -> (id, List.map snd coords))
    |> List.map (fun (id, coords) -> (id, List.map (fun c -> Map.find c coordCounts) coords))
    |> List.filter (fun (_, counts) -> List.forall (fun x -> x = 1) counts)
    |> List.head
    |> fst    

let main() = 
    readInputFile()
    |> List.map (parseClaim >> claimToIdCoords)
    |> List.concat
    |> findSingleArea
    |> printfn "Answer is: %i"