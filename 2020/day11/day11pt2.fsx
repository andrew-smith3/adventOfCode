open System

let parse () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map Seq.toArray
    |> Seq.toArray
    |> array2D

let hasSeatInView (spots: char[,]) (row: int) (col: int) ((modR, modC) : int * int) = 
    let rows = Array2D.length1 spots
    let cols = Array2D.length2 spots
    let mutable found = '.'
    let mutable r = row + modR
    let mutable c = col + modC
    while found = '.' && r >= 0 && c >= 0 && r < rows && c < cols do
        found <- spots.[r,c]
        r <- r + modR
        c <- c + modC
    found = '#'

let seatsInView (spots: char[,]) (row: int) (col: int) = 
    let modifiers = [|-1; 0; 1|]
    Array.allPairs modifiers modifiers 
    |> Array.filter (fun (x, y) -> Math.Abs(x) + Math.Abs(y) > 0)
    |> Array.map (hasSeatInView spots row col)
    |> Array.filter id
    |> Array.length

let emptyToOccupied (spots: char [,]) (row: int) (col: int) = 
    (seatsInView spots row col)
    |> fun x -> if x = 0 then '#' else 'L'
 
let occupiedToEmpty (spots: char [,]) (row: int) (col: int) =
    (seatsInView spots row col)
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