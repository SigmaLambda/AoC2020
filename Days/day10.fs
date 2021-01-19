module Day10

let ReadLinesToInt dayNr =
    System.IO.File.ReadAllLines(".//input//Day" + dayNr + ".txt")
    |> Array.map int
    |> Array.toList

let testinput = ReadLinesToInt "10"
let outlet = 0
let DeviceMax = List.max testinput + 3

let orderedInput =
    List.rev (DeviceMax :: List.rev (List.sort testinput))

let rec Count13 prev ones threes input =

    match input with
    | [] -> (ones, threes, ones * threes)
    | hd :: tl ->
        let diff = hd - prev
        match diff with
        | 1 -> Count13 hd (ones + 1) threes tl
        | 3 -> Count13 hd ones (threes + 1) tl
        | _ -> failwith "Error in Count13"

