let parse () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map Seq.toArray
    |> Seq.toArray
    |> array2D

let emptyToOccupied (spots: char [,]) (row: int) (col: int) = 
    spots.[row-1..row+1, col-1..col+1]
    |> Seq.cast<char>
    |> Seq.filter (fun x -> x = '#')
    |> Seq.length
    |> fun x -> if x = 0 then '#' else 'L'
 
let occupiedToEmpty (spots: char [,]) (row: int) (col: int) =
    spots.[row-1..row+1, col-1..col+1]
    |> Seq.cast<char>
    |> Seq.filter (fun x -> x = '#')
    |> Seq.length
    |> fun x -> if x >= 5 then 'L' else '#'

let m (spots: char [,]) (row: int) (col: int) (ch: char) =
    match ch with
    | '.' -> '.'
    | '#' -> occupiedToEmpty spots row col
    | 'L' -> emptyToOccupied spots row col
    | x -> failwithf "Unhandled char: %c" x

let rec settle (spots: char [,]) = 
    let newSpots = spots |> Array2D.mapi (m spots)
    if spots = newSpots then spots else settle newSpots
    
let count (spots: char [,]) = 
    spots 
    |> Seq.cast<char>
    |> Seq.filter (fun x -> x = '#')
    |> Seq.length


let main () = 
    parse ()
    |> settle
    |> count
    |> printfn "Answer: %d"

main ()