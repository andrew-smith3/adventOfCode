open System

type Status =
    | Parsing of string * bool
    | Searching

type State = {
    Status: Status
    Sum: int
    Schematic: string array
    MaxX: int
    MaxY: int
}
with
    static member Init(schematic: string array) =
        let maxX = schematic[0].Length - 1
        let maxY = schematic.Length - 1
        { Status = Searching; Sum = 0; Schematic = schematic; MaxX = maxX; MaxY = maxY }


let isAroundSymbol (state: State) (x: int) (y: int) =
    [|(x - 1, y - 1); (x - 1, y); (x - 1, y + 1); (x, y - 1); (x, y + 1); (x + 1, y - 1); (x + 1, y); (x + 1, y + 1)|]
    |> Array.filter (fun (x, y) -> x >= 0 && x <= state.MaxX && y >= 0 && y <= state.MaxY)
    |> Array.exists (fun (x, y) ->
        let c = state.Schematic[y][x]
        not (Char.IsDigit(c)) && c <> '.'
        )

let folder (y: int) (state: State) ((i, c): int * char) =
    match state.Status with
    | Searching when Char.IsDigit(c) ->
        let aroundSymbol = isAroundSymbol state i y
        { state with Status = Parsing ((c.ToString(), aroundSymbol)) }
    | Parsing (s, aroundSymbol) when Char.IsDigit(c) ->
        let aroundSymbol = if aroundSymbol then true else isAroundSymbol state i y
        { state with Status = Parsing (s + c.ToString(), aroundSymbol) }
    | Parsing (s, aroundSymbol) ->
        let add = if aroundSymbol then Int32.Parse(s) else 0
        { state with Status = Searching; Sum = state.Sum + add }
    | Searching -> state


let processLine (state: State) ((i, line): int * string) =
    line
    |> Seq.indexed
    |> Seq.fold (folder i) state

let main () =
    let input = System.IO.File.ReadAllLines("input.txt")
    let state = State.Init(input)
    input
    |> Array.indexed
    |> Array.fold processLine state
    |> _.Sum
    |> fun x -> printfn $"Answer: {x}"
    
    
main()