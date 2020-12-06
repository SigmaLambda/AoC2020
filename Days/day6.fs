module Day6

open Day4

let testinput =
    System.IO.File.ReadAllLines ".//input//Day6Test.txt"
    |> Array.map (fun s -> s + ";")
    |> Array.toList

let input =
    System.IO.File.ReadAllLines ".//input//day6.txt"
    |> Array.map (fun s -> s + ";")
    |> Array.toList


let GetSumCount puzzle =
    puzzle
    |> Day4.groupData [] []
    |> List.map (fun x ->
        x.ToCharArray()
        |> Array.filter (fun x -> (x <> ';'))
        |> Array.distinct)
    |> List.map (fun x -> x.Length)
    |> List.sum


let Day6 () =
    printfn "Day6Test: %d " (GetSumCount testinput)
    printfn "Day6: %d " (GetSumCount input)

let getEveryOne (a: (char * int) array) =
    let _, semiCount = Array.find (fun (c, i) -> c = ';') a
    Array.filter (fun (c, i) -> i = semiCount) a
    |> Array.length
    |> (+) -1

let countEveryOne (puzzle: string list) =
    puzzle
    |> Day4.groupData [] []
    |> List.map (fun (s: string) -> s.ToCharArray())
    |> List.map (fun a -> Array.countBy id a)
    |> List.map getEveryOne
    |> List.sum

let Day6b () =
    printfn "Day6b: %d " (countEveryOne input)
