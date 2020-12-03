let parseRange (s: string) = 
    let parts = s.Split('-')
    let min = parts.[0]
    let max = parts.[1]
    (int min, int max)

let isValid (s: string) = 
    let parts = s.Replace(":", "").Split(' ')
    let first, last = parseRange parts.[0]
    let character = parts.[1].[0]
    let input = parts.[2]
    if (input.[first-1] = character) <> (input.[last-1] = character)
    then 1
    else 0


let main () = 
    let count = Seq.sumBy isValid (System.IO.File.ReadLines("input.txt"))
    printfn "Count: %d" count


main ()