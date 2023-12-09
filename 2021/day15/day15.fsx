open System
open System.Collections.Generic

let readFile () = System.IO.File.ReadLines("test.txt")

let charToInt (c: char) = int c - int '0'

let parse (s: seq<string>) =
    s
    |> Seq.map (Seq.map charToInt)
    |> array2D

let findNeighbors (i:int) (j: int) (numbers: int[,]) =
    let iMax = (Array2D.length1 numbers) - 1
    let jMax = (Array2D.length2 numbers) - 1
    [|
        if i > 0 then yield i - 1, j
        if i < iMax then yield i + 1, j
        if j > 0 then yield i, j - 1
        if j < jMax then yield i, j + 1
    |]

let distance (x1: int) (y1: int) (x2: int) (y2: int) =
    Math.Abs(x1 - x2) + Math.Abs(y1 - y2)

let mutable count = 0

let rec findPathHelper (map: int[,]) (queue: PriorityQueue<int * int * int, int>) =
    count <- count + 1
    let x, y, risk = queue.Dequeue()
    printfn $"x: %d{x}, y: %d{y}, risk: %d{risk}"
    let goalX = Array2D.length1 map - 1
    let goalY = Array2D.length2 map - 1
    if x = goalX && y = goalY
    then risk
    else 
        findNeighbors x y map
        |> Array.filter( fun (i, j) -> (distance i j goalX goalY) <= (distance x y goalX goalY))
        |> Array.iter (fun (i, j) ->
            let newRisk = risk + map[i,j]
            let priority = newRisk + (distance i j goalX goalY)
            let inQueue =
                queue.UnorderedItems
                |> Seq.tryFind (fun struct ((m, n, _), _) -> m = i && n = j)
            match inQueue with
            | Some (_, p) ->
                if priority > p then queue.Enqueue((i, j, newRisk), priority)
            | None _ -> 
                queue.Enqueue((i, j, newRisk), priority)
            )
        findPathHelper map queue
    

let findPath (map: int[,]) =
    let queue = PriorityQueue<int * int * int, int>()
    printfn $"Map: %A{map}"
    queue.Enqueue((0,0, 0), 0)
    findPathHelper map queue

let main () =
    readFile ()
    |> parse
    |> findPath
    |> fun x -> printfn $"Answer: %d{x}, Count: %d{count}"
    
main ()