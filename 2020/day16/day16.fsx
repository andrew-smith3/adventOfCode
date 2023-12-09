open System

type Rule = {
    Name: string
    Ranges: (int * int)[]
}

let lineToRule (s: string) =
    let [|name; rest|] = s.Split(": ")
    let ranges =
        rest.Split(" or ")
        |> Array.map (fun x -> x.Split("-") |> Array.map int |> fun y -> (y[0], y[1]))
    {Name = name; Ranges = ranges}

let lineToTicket (s: string) = s.Split(",") |> Array.map int

let parse () =
    let i = ref 0;
    System.IO.File.ReadLines("input.txt")
    |> Seq.groupBy (fun x ->
        if String.IsNullOrEmpty(x) then i.Value <- i.Value + 1
        i)
    |> Seq.map snd
    |> Seq.toList
    |> fun s ->
        let rules =
            List.item 0 s
            |> Seq.map lineToRule
        // let ticket =
        //     List.item 1 s
        //     |> Seq.skip 1
        //     |> Seq.head
        //     |> lineToTicket
        let otherTickets =
            List.item 2 s
            |> Seq.skip 1
            |> Seq.map lineToTicket
            |> Seq.toArray
        (rules, otherTickets)    
        
let rec scanningRate ((rules, otherTicekt)) =
    
    
        
let main () =
    parse ()
    |> scanningRate