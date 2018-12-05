open System

type Event = 
    | BeginsShift of int
    | FallsAsleep
    | WakesUp

type Record = { dateTime: DateTime; event: Event}

type State = { 
    currentGuard: int; 
    fellAsleep: int option; 
    guardLog: Map<int, int list>
}

let readInputFile () = 
    System.IO.File.ReadAllLines("./2018/day4/input.txt") 
    |> Array.toList

let parseEvent (s: string) = 
    match s.[0] with
    | 'G' -> 
        s.Split([|' '|])
        |> Array.item 1
        |> (fun s -> s.Replace("#", ""))
        |> int
        |> BeginsShift
    | 'f' -> FallsAsleep
    | 'w' -> WakesUp
    | _ -> failwith "Invalid input"    

let parseRecord (s: string) = 
    let dateTime = DateTime.Parse(s.Substring(1, 16))
    let event = parseEvent (s.Substring(19))
    {dateTime = dateTime; event = event}

let logGuardMinutes (state: State) (record: Record) : State = 
    match record.event with
    | BeginsShift i -> 
        let guardLog = 
            match Map.tryFind i state.guardLog with
            | Some _ -> state.guardLog
            | None -> Map.add i [] state.guardLog
        {state with currentGuard=i; fellAsleep=None; guardLog=guardLog}
    | FallsAsleep -> {state with fellAsleep = Some record.dateTime.Minute}
    | WakesUp -> 
        let fellAsleepAt = 
            match state.fellAsleep with
            | Some i -> i
            | None -> failwith "Can't wake when not asleep"
        let range = [fellAsleepAt..record.dateTime.Minute-1]
        let minuteList = Map.find state.currentGuard state.guardLog
        let list = List.append range minuteList
        let guardLog = Map.add state.currentGuard list state.guardLog
        {state with fellAsleep=None; guardLog=guardLog}

let findGuard (state: State) = 
    state.guardLog
    |> Map.toList
    |> List.sortByDescending (fun (g, m) -> List.length m)
    |> List.head
    |> fun (g, m) -> 
        let minute = List.countBy id m |> List.maxBy snd |> fst
        g * minute

let main() = 
    readInputFile()
    |> List.map parseRecord
    |> List.sortBy (fun x -> x.dateTime)
    |> List.fold logGuardMinutes {currentGuard=0; fellAsleep=None; guardLog=Map.empty}
    |> findGuard
    |> printfn "Answer is: %i"
