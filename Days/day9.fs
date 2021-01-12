module Day9

let rec Combinate (numbers: int64 list) (combinations: int64 list): int64 list =
    match numbers with
    | [] -> combinations
    | hd :: tl ->
        let newCombos = (List.map (fun x -> hd + x) tl)
        Combinate tl (List.append newCombos combinations)


let testinput =
    System.IO.File.ReadAllLines ".//input//Day9test1.txt"
    |> Array.map int
    |> Array.toList


let rec findFirst preamble (numbers: int64 list) =
    match numbers with
    | [] -> -1
    | hd :: tl ->
        let combinations = Combinate numbers.[..(preamble - 1)] []
        //printfn "%A" combinations
        match List.tryFind (fun x -> x = numbers.[preamble]) combinations with
        | None -> int numbers.[preamble]
        | Some n -> findFirst preamble numbers.[1..]

let p1Input =
    System.IO.File.ReadAllLines ".//input//day9.txt"
    |> Array.map int64
    |> Array.toList

let Day9Run () = printfn "%d" (findFirst 25 p1Input)

let rec AddNumbers (target: int64) (acc: int64) (index: int) (numbers: int64 list) =
    let next = numbers.[index] + acc

    if next = target then
        let series = numbers.[0..index]
        Seq.min series + Seq.max series
    else if next > target then
        AddNumbers target (int64 0) 0 (numbers.[1..])
    else
        AddNumbers target next (index + 1) numbers

let Day9BRun () =
    printfn "9b: %d" (AddNumbers (int64 373803594) (int64 0) 0 p1Input)
