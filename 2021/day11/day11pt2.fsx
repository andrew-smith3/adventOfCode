let readFile () =
    System.IO.File.ReadLines("input.txt")
    |> Seq.map (Seq.map (int >> (+) -48) >> Seq.toArray)
    |> array2D
    
let allPoints array2D =
    seq { for x in [0..(Array2D.length1 array2D) - 1] do
            for y in [0..(Array2D.length2 array2D) - 1] do
                yield (x, y) }

let findNeighbors (numbers: int[,]) ((i, j): int * int)  =
    let xMax = (Array2D.length1 numbers) - 1
    let yMax = (Array2D.length2 numbers) - 1
    seq { for x in [i - 1..i + 1] do
            for y in [j - 1..j + 1] do
            if x >= 0 && x <= xMax && y >= 0 && y <= yMax then yield (x, y) }
    |> Seq.toList
    
let rec processFlashes (map: int[,]) (flashed: (int * int) list) =
    let newlyFlashed =
        allPoints map
        |> Seq.filter (fun (x, y) -> map.[x, y] >= 10 && not (List.contains (x,y) flashed))
        |> Seq.toList
    match newlyFlashed with
    | [] -> if List.length flashed = 100 then true else false
    | l -> 
        l
        |> List.collect (findNeighbors map) 
        |> List.iter (fun (x, y) -> map.[x,y] <- map.[x,y] + 1)
        let flashed = List.append newlyFlashed flashed
        processFlashes map flashed
    
let rec processStep (step: int) (map: int[,]) =
    let m = Array2D.map ((+) 1) map
    match processFlashes m [] with
    | true -> step + 1
    | false -> 
        let m = Array2D.map (fun x -> if x > 9 then 0 else x) m
        processStep (step + 1) m
    
let main () =
    readFile ()
    |> processStep 0
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()