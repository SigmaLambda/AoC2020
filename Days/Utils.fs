module Utils

//Gets all possible unique combinations from a list
let rec CreateCombinations numbers acc =
    match numbers with
    | [] -> acc
    | hd :: tl ->
        let newCombos = List.map (fun x -> hd + x) tl
        CreateCombinations tl (newCombos @ acc)

let ReadLinesToInt dayNr =
    System.IO.File.ReadAllLines(".//input//Day" + dayNr + ".txt")
    |> Array.map int
    |> Array.toList
