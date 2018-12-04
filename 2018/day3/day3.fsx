type Claim = {
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
    | [|_; _; coord; dimensions;|] -> 
        let c = coord.Replace(":", "").Split([|','|])
        let left = Array.item 0 c |> int
        let top =  Array.item 1 c |> int
        let d = dimensions.Split([|'x'|])
        let width = Array.item 0 d |> int
        let height = Array.item 1 d |> int
        {left=left; top=top; width=width; height=height;}
    | _ -> failwith "Error"

let claimToCoords (claim: Claim) : (int * int) list = 
    let lastX = claim.left + claim.width - 1
    let lastY = claim.top + claim.height - 1
    [for i in claim.left .. lastX do
        for j in claim.top .. lastY -> (i, j)]

let countDoubledAreas (coords: (int * int) list) = 
    List.countBy id coords
    |> List.map snd
    |> List.filter (fun x -> x > 1)
    |> List.length

let main() = 
    readInputFile()
    |> List.map (parseClaim >> claimToCoords)
    |> List.concat
    |> countDoubledAreas
    |> printfn "Answer is: %i"