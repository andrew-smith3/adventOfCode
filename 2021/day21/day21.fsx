let player1Start = 10
let player2Start = 4

let dice = Seq.initInfinite (fun index -> index % 100 + 1)

let gameState = ((player1Start, 0), (player2Start, 0))

let rec game (((p1Pos, p1Score), (p2Pos, p2Score), dicePos): (int * int) * (int * int) * int) =
    let roll = dice |> Seq.skip dicePos |> Seq.take 3 |> Seq.sum
    let dicePos = dicePos + 3
    let p1Pos = (p1Pos + roll) % 10
    let p1Pos = if p1Pos = 0 then 10 else p1Pos
    let p1Score = p1Pos + p1Score
    printfn $"roll: %d{roll}, p1Pos: %d{p1Pos}, p1Score: %d{p1Score}"
    if p1Score >= 1000
    then (p2Score, dicePos)
    else
        let roll = dice |> Seq.skip dicePos |> Seq.take 3 |> Seq.sum
        let dicePos = dicePos + 3
        let p2Pos = (p2Pos + roll) % 10
        let p2Pos = if p2Pos = 0 then 10 else p2Pos
        let p2Score = p2Pos + p2Score
        if p2Score >= 1000
        then (p1Score, dicePos)
        else
            game ((p1Pos, p1Score), (p2Pos, p2Score), dicePos)

let main () =
    game ((player1Start, 0), (player2Start, 0), 0)
    |> fun (x, y) -> x * y
    |> fun x -> printfn $"Answer: %d{x}"
    
main ()