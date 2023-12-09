
type Node =
    | Dir of Directory
    | File of string * int
and Directory = {
    Name: string
    Children: Node list
    Path: string list
}

type State = {
    CurrentDir: Directory
}

let root = {
    Name = "/"
    Children = []
    Path = []
}

let (|StartsWith|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None
        
let getDirectory (nodes: Node list) (name: string) =
    nodes
    |> List.choose (function Dir d -> Some d | _ -> None)
    |> List.find (fun d -> d.Name = name)
    
let goUpDirectory (path: string list) =
    printfn "Path: %A" path
    printfn "Root: %A" root
    path
    |> List.rev
    |> List.fold (fun d -> getDirectory d.Children) root
        
let changeDirectory (dir: string) (currentDir: Directory) =
    match dir with
    | "/" -> root
    | ".." ->  goUpDirectory currentDir.Path
    | x ->
        printfn "%s" x
        getDirectory currentDir.Children x
        

let f (state: Directory) (line: string) =
    printfn "d: %A, line: %s" state line
    match line with
    | StartsWith "$ cd " dir -> changeDirectory dir state
    | "$ ls" -> state
    | StartsWith "dir " dir ->
        let newDirectory = Dir { Name = dir; Children = []; Path = dir::state.Path }
        {state with Children = newDirectory::state.Children }
    | x -> 
        let parts = x.Split(" ")
        let size = int parts[0]
        let file = File (parts[1], size)
        {state with Children = file::state.Children}

let rec traverse ((sum, dirs): int * int list) (current: Node) =
    match current with
    | File (_, x ) -> (sum + x, dirs)
    | Dir d ->
        let (size, newDirs) =
            d.Children
            |> List.fold traverse (0, dirs)
        if size <= 100000 then (size + sum, size::newDirs) else (size + sum, newDirs)

let main () =
    System.IO.File.ReadAllLines("testInput.txt")
    |> Array.fold f root
    |> changeDirectory "/"
    |> Dir
    |> traverse (0, [])
    |> snd
    |> List.sum
    |> printfn "%d"

main ()