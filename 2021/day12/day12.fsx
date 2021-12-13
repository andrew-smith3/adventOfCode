open System
let readFile () = System.IO.File.ReadLines("input.txt")


let addConnection (id: string) (connection: string) (nodes: Map<string, string list>) =
    match Map.tryFind id nodes with
    | None ->
        Map.add id [connection] nodes
    | Some l ->
        Map.add id (connection::l) nodes

let createMap (nodeMap: Map<string, string list>) (s: string) =
    let nodes = s.Split "-"
    let node1 = nodes.[0]
    let node2 = nodes.[1]
    nodeMap
    |> addConnection node1 node2
    |> addConnection node2 node1
    
let rec findPaths (nodeMap: Map<string, string list>) (path: string list) (completed: string list list) (nodeId: string) =
    match nodeId with
    | "end" ->
        let fullPath = "end"::path
        fullPath::completed
    | x ->
        Map.find nodeId nodeMap
        |> List.filter ((fun x -> (List.contains x path) && Char.IsLower(x, 0)) >> not)
        |> List.fold (findPaths nodeMap (nodeId::path)) completed
    

let main () =
    readFile ()
    |> Seq.fold createMap Map.empty
    |> fun x -> findPaths x [] [] "start"
    |> List.length
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()