
let toIntArray (s: string) =
    s
    |> Seq.map int
    |> Seq.toArray

let process (grid: int[][]) =
    let height = Array2D.length1 grid
    let width = Array2D.length2 grid
    
    

let main () =
    System.IO.File.ReadAllLines("testInput.txt")
    |> Array.map toIntArray
    |> process

main ()