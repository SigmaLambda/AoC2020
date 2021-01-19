module Day11

let test11 = [ 1721; 979; 366; 299; 675; 1456 ]

//F# loop
let f2 () =

    for i in 0 .. (test11.Length - 1) do
        for j in 0 .. (test11.Length - 1) do
            if test11.[i] + test11.[j] = 2020 then printfn "%d" (test11.[i] * test11.[j])

f2 ()


//Recursive
let rec f () =
    let r1 =
        int
            ((System.Random()).NextDouble()
             * float test11.Length)

    let r2 =
        int
            ((System.Random()).NextDouble()
             * float test11.Length)

    printfn "%d %d" r1 r2
    if test11.[r1] + test11.[r2] = 2020 then printfn "%d" (test11.[r1] * test11.[r2]) else f ()

f ()


let input =
    System.IO.File.ReadAllLines "./Input/Day1.txt"
    |> Array.map int
    |> Array.toList