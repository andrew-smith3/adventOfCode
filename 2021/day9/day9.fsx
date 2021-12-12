let readFile () = System.IO.File.ReadLines("input.txt")

let parse (s: seq<string>) =
    s
    |> Seq.map (Seq.map (int >> (+) -48) >> Seq.toArray)
    |> Seq.toArray
    |> array2D

let flattenArray2D array2D =
    seq { for x in [0..(Array2D.length1 array2D) - 1] do
                for y in [0..(Array2D.length2 array2D) - 1] do
                    yield array2D.[x, y] }

let findNeighbors (i:int) (j: int) (numbers: int[,]) =
    let iMax = (Array2D.length1 numbers) - 1
    let jMax = (Array2D.length2 numbers) - 1
    [|
        if i > 0 then yield numbers.[i - 1, j]
        if i < iMax then yield numbers.[i + 1, j]
        if j > 0 then yield numbers.[i, j - 1]
        if j < jMax then yield numbers.[i, j + 1]
    |]

let findRiskLevel (map: int [,]) =
    map
    |> Array2D.mapi (fun i j v ->
        let neighbors = findNeighbors i j map
        if Array.forall (fun x -> x > v) neighbors then v + 1 else 0
        )
    |> flattenArray2D
    |> Seq.sum

let main () =
    readFile ()
    |> parse
    |> findRiskLevel
    |> fun x -> printfn $"Answer: %d{x}"
    
    
main ()