open System

let setStack (stacks: Map<int, char list>) ((i, c): int * char) =
    let idx = (i / 4) + 1
    stacks
    |> Map.tryFind idx
    |> fun x -> if x.IsNone then [] else x.Value
    |> fun x -> c::x
    |> fun x -> Map.add idx x stacks

let f (stacks: Map<int, char list>) (line: string) =
    line
    |> Seq.indexed
    |> Seq.where (fun x -> (fst x) % 4 = 1 && Char.IsLetter(snd x))
    |> Seq.fold setStack stacks

let makeMoves (stacks: Map<int, char list>) (move: int[]) =
    let a = move[0]
    let mutable from = Map.find move[1] stacks
    let mutable dest = Map.find move[2] stacks
    for i in 1 .. a do
        let h = List.head from
        from <- List.tail from
        dest <- h::dest
    stacks
    |> Map.add move[1] from
    |> Map.add move[2] dest

let compute (lines: string[]) =
    let stacks =
        lines
        |> Array.takeWhile (fun (l) -> l.StartsWith(" 1 ") |> not)
        |> Array.rev
        |> Array.fold f Map.empty

    lines
    |> Array.where (fun l -> l.StartsWith "move")
    |> Array.map (fun x -> x.Replace("move ", "").Replace("from ", "").Replace("to ", "").Split(" ") |> Array.map int)
    |> Array.fold makeMoves stacks
    |> Map.toArray
    |> Array.map (snd >> List.head)
    |> String
    
let main () =
    System.IO.File.ReadAllLines("input.txt")
    |> compute
    |> printfn "%s"

main ()