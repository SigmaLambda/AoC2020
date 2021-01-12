module Day1

let test1 = [ 1721; 979; 366; 299; 675; 1456 ]


let input =
    System.IO.File.ReadAllLines "./Input/Day1.txt"
    |> Array.map int
    |> Array.toList


let products list1 =
    List.collect (fun x -> List.map (fun y -> if x + y = 2020 then Some(x * y) else None) list1) list1
    |> List.choose id
    |> List.head


let productsThree list1 =
    List.collect (fun x ->
        List.collect (fun z -> List.map (fun y -> if x + y + z = 2020 then Some(x * y * z) else None) list1) list1)
        list1
    |> List.choose id
    |> List.head

let RunDay1 () =

    printfn "testAns1: %d" (products test1)

    printfn "Day1a: %d" (products input)

    printfn "Day1b: %d" (productsThree input)


// let productsThree list1 =
//     List.collect (fun x ->
//         List.collect (fun z -> List.map (fun y -> if x + y + z = 2020 then Some(x * y * z) else None) list1) list1)
//         list1
//     |> List.choose id
//     |> List.head










let inputs =
    System.IO.File.ReadAllLines "./Input/Day1.txt"
    |> Array.map int

// let mutable result = 0
// let BetterDay1 someInput =
//     someInput
//     |> Array.collect (fun a ->
//         someInput
//         |> Array.collect (fun b ->
//             someInput
//             |> Array.map (fun c -> if a + b + c = 2020 then result <- a*b*c)))
//     |> Array.choose id
//     |> Array.head
//     |> printfn "Better day1: %d"
