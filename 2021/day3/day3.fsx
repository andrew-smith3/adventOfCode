open System

let readFile () = 
    System.IO.File.ReadLines("input.txt")

let calc (lines: string[]) =
    let length = Array.length lines
    let lineLength = Array.head lines |> String.length
    lines
    |> Array.map (Seq.map (int >> (+) -48) >> Seq.toArray)
    |> Array.fold (Array.map2 (+) ) (Array.zeroCreate lineLength)
    |> Array.map (fun x -> if x > length / 2 then 1 else 0)
    |> Array.fold (fun s x -> (s <<< 1) ||| x) 0
    |> uint
    |> fun g ->
        let mask = ~~~(~~~0u <<< lineLength)
        let e = ~~~g &&& mask
        e * g
let main () =
    readFile ()
    |> Seq.toArray
    |> calc
    |> fun x -> printfn $"Answer: %d{x}"

main ()
