type Bag = {
    below: int list
    above: int list
}


let r (bag: Bag) (i: int) = 
    if i < 1010
    then {bag with below = i::bag.below}
    else {bag with above = i::bag.above}

let readFile () = 
    System.IO.File.ReadLines("input.txt")
    |> Seq.map int
    |> Seq.fold r {above = []; below = []}
    
let rec findNumber (bag:Bag) = 
    match bag.below with
    | i::rest ->
        if List.contains (2020 - i) bag.above
        then i
        else findNumber {bag with below = rest}
    | [] -> failwith "Unable to find number"


let main () = 
    let bag = readFile ()
    let number = findNumber bag
    let answer = number * (2020 - number)
    printfn "Answer: %d" answer


main ()