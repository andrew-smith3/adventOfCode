open System

type Status =
    | Parsing of string * (int * int) list
    | Searching

type Number = {
    Value: int
    Gears: (int * int) list
}

type State = {
    Status: Status
    Numbers: Number list
    Schematic: string array
    MaxX: int
    MaxY: int
}
with
    static member Init(schematic: string array) =
        let maxX = schematic[0].Length - 1
        let maxY = schematic.Length - 1
        { Status = Searching; Numbers = []; Schematic = schematic; MaxX = maxX; MaxY = maxY }


let findGears (state: State) (x: int) (y: int) =
    [|(x - 1, y - 1); (x - 1, y); (x - 1, y + 1); (x, y - 1); (x, y + 1); (x + 1, y - 1); (x + 1, y); (x + 1, y + 1)|]
    |> Array.filter (fun (x, y) -> x >= 0 && x <= state.MaxX && y >= 0 && y <= state.MaxY)
    |> Array.filter (fun (x, y) ->
        let c = state.Schematic[y][x]
        c = '*')
    |> Array.toList

let folder (y: int) (state: State) ((i, c): int * char) =
    match state.Status with
    | Searching when Char.IsDigit(c) ->
        let gears = findGears state i y
        { state with Status = Parsing ((c.ToString(), gears)) }
    | Parsing (s, gears) when Char.IsDigit(c) ->
        let gears =
            findGears state i y
            |> (@) gears
            |> List.distinct
        { state with Status = Parsing (s + c.ToString(), gears) }
    | Parsing (s, gears) ->
        let number = { Value = Int32.Parse(s); Gears = gears }
        { state with Status = Searching; Numbers = number::state.Numbers }
    | Searching -> state


let processLine (state: State) ((i, line): int * string) =
    line
    |> Seq.indexed
    |> Seq.fold (folder i) state

let toMap (s: State) =
    
    let folder (m : Map<(int * int), int list>) ((g, n): (int * int) * int) =
        match m.TryFind(g) with
        | Some l ->
            m.Add(g, (n::l))
        | None ->
            m.Add(g, [n])
    
    s.Numbers
    |> List.collect (fun n -> n.Gears |> List.map(fun g -> (g, n.Value)))
    |> List.fold folder Map.empty
    |> Map.filter (fun g n -> (List.length n) = 2)
    |> Map.map (fun g (a::b::_) -> a * b)
    |> Map.fold (fun s _ a -> s + a) 0

let main () =
    let input = System.IO.File.ReadAllLines("input.txt")
    let state = State.Init(input)
    input
    |> Array.indexed
    |> Array.fold processLine state
    |> toMap
    |> fun x -> printfn $"Answer: {x}"
    
    
main()