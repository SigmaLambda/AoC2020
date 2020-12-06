module Day5

type PlaneSeat = { Row: int; Column: int; SeatId: int }

let CreatePlaneSeat r c =
    let s = (r * 8) + c
    { Row = r; Column = c; SeatId = s }

let HalfList (l: int list) =
    // printfn "%A" l
    let splitAt = (l.Length / 2) - 1
    (l.[..splitAt], l.[(splitAt + 1)..])

let rec FindRow input list =
    match input with
    | [] ->
        // printfn "%A" list
        List.head list
    | [ last ] ->
        match last with
        | 'F' -> list.Head
        | 'B' -> list.Tail.Head
        | _ -> failwith "Error in last Findrow"
    | hd :: tl ->
        let lower, higher = HalfList list
        match hd with
        | 'F' ->
            // printfn "%A" lower
            FindRow tl lower
        | 'B' ->
            // printfn "%A" higher
            FindRow tl higher
        | _ -> failwith "Error in Findrow"

let rec FindColumn input list =
    match input with
    | [] ->
        // printfn "%A" list
        List.head list
    | [ last ] ->
        match last with
        | 'L' -> list.Head
        | 'R' -> list.Tail.Head
        | _ -> failwith "Error in last FindColumn"
    | hd :: tl ->
        let lower, higher = HalfList list
        match hd with
        | 'L' -> FindColumn tl lower
        | 'R' -> FindColumn tl higher
        | _ -> failwith "Error in FindColumn"


let test1 =
    "BFFFBBFRRR".ToCharArray() |> Array.toList

let test2 =
    "FFFBBBFRRR".ToCharArray() |> Array.toList

let test3 =
    "BBFFBBFRLL".ToCharArray() |> Array.toList



let ans1 = { Row = 70; Column = 7; SeatId = 567 }
let ans2 = { Row = 14; Column = 7; SeatId = 119 }
let ans3 = { Row = 102; Column = 4; SeatId = 820 }


let rows = [ 0 .. 127 ]
let columns = [ 0 .. 7 ]

let TestA () =
    let r1 = FindRow test1.[..6] rows
    let c1 = FindColumn test1.[7..] columns
    let s1 = CreatePlaneSeat r1 c1
    printfn "r1: %d c1: %d s: %A" r1 c1 s1

    let r2 = FindRow test2.[..6] rows
    let c2 = FindColumn test2.[7..] columns
    let s2 = CreatePlaneSeat r2 c2
    printfn "r2: %d c2: %d s: %A" r2 c2 s2

    let r3 = FindRow test3.[..6] rows
    let c3 = FindColumn test3.[7..] columns
    let s3 = CreatePlaneSeat r3 c3
    printfn "r3: %d c3: %d s: %A" r3 c3 s3


let input =
    System.IO.File.ReadAllLines "./Input/day5.txt"
    |> Array.toList
    |> List.map (fun x -> x.ToCharArray() |> Array.toList)



let Day5A () =
    let seats =
        input
        |> List.map (fun cs ->
            let r = FindRow cs.[..6] rows
            let c = FindColumn cs.[7..] columns
            CreatePlaneSeat r c)

    printfn "Highest: %A" (List.maxBy (fun s -> s.SeatId) seats)

    printfn
        "%A"
        (List.sortBy (fun x -> x.SeatId) seats
         |> List.map (fun s -> s.SeatId))



let FindGap (seats: PlaneSeat list) =
    let map =
        let paired = List.map (fun ss -> ss.SeatId, ss) seats
        Map.ofList paired

    let max =
        List.maxBy (fun x -> x.SeatId) seats
        |> fun p -> p.SeatId

    let min =
        List.minBy (fun x -> x.SeatId) seats
        |> fun p -> p.SeatId

    for i in min .. max do
        let s = map.TryFind i
        match s with
        | None -> printf "Empty: %d" i
        | Some s -> ()

let Day5B () =
    let seats =
        input
        |> List.map (fun cs ->
            let r = FindRow cs.[..6] rows
            let c = FindColumn cs.[7..] columns
            CreatePlaneSeat r c)

    FindGap seats
