
let allPoints array2D =
    seq { for x in [0..(Array2D.length1 array2D) - 1] do
            for y in [0..(Array2D.length2 array2D) - 1] do
                yield (x, y) }
        
let nextUnmarkedPoint array2D =
    allPoints array2D |> Seq.tryFind (fun (x, y) -> array2D.[x,y] = 0)
    
let groupBasinPoints (basins: int [,]) =
    allPoints basins |> Seq.groupBy (fun (i, j) -> basins.[i, j])

let readFile () = System.IO.File.ReadLines("input.txt")

let parse (s: seq<string>) =
    s
    |> Seq.map (Seq.map (int >> (+) -48) >> Seq.toArray)
    |> Seq.toArray
    |> array2D
    
let findNeighbors (i:int) (j: int) (numbers: int[,]) =
    let iMax = (Array2D.length1 numbers) - 1
    let jMax = (Array2D.length2 numbers) - 1
    [
        if i > 0 then yield i - 1, j
        if i < iMax then yield i + 1, j
        if j > 0 then yield i, j - 1
        if j < jMax then yield i, j + 1
    ]
   
let rec markBasin (map: int [,]) (basins: int [,]) (id: int) ((x, y): int * int) =
    let v = map.[x,y]
    match v with
    | 9 ->
        basins.[x,y] <- 9
    | _ ->
        basins.[x,y] <- id
        findNeighbors x y basins
        |> List.filter (fun (i, j) -> basins.[i,j] = 0)
        |> List.iter (fun coord -> markBasin map basins id coord)


let rec markBasins (map: int [,]) (basins: int [,]) (id: int) =
    match nextUnmarkedPoint basins with
    | None -> basins
    | Some point ->
        markBasin map basins id point
        markBasins map basins (id+1)
    

let main () =
    let map = readFile () |> parse
    let basins : int[,] = Array2D.zeroCreate (Array2D.length1 map) (Array2D.length2 map)
    markBasins map basins 1
    |> groupBasinPoints
    |> Seq.filter (fun (k, v) -> k <> 9)
    |> Seq.map (snd >> Seq.length)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (*)
    |> fun x -> printfn $"Answer: %d{x}"
    
    
main ()