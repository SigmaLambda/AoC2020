module Day2

let input =
    System.IO.File.ReadAllLines "./input/Day2.txt"
    |> Array.toList

let test1 =
    [ "1-3 a: abcde"
      "1-3 b: cdefg"
      "2-9 c: ccccccccc" ]

let splitToTuple (s: string) (c: string) =
    let ans = s.Split(c)
    (ans.[0], ans.[1])


let splitRow s =
    let rules, pwd = splitToTuple s ": "
    let minmax, c = splitToTuple rules " "
    let min, max = splitToTuple minmax "-"
    int min, int max, char c, pwd.ToCharArray()

let checkPwd min max c (pwd: char array) =
    let occurances =
        Array.filter (fun x -> x = c) pwd |> Array.length

    occurances >= min && occurances <= max


let Day2a () =
    let res =
        test1
        |> List.filter (fun x ->
            let a, b, c, d = splitRow x
            checkPwd a b c d)

    printfn "Test1: %d" res.Length

    let res =
        input
        |> List.filter (fun x ->
            let a, b, c, d = splitRow x
            checkPwd a b c d)

    printfn "Day2a: %d" res.Length


let checkPwd2b min max c (pwd: char array) =
    let a = pwd.[min - 1] = c
    let b = pwd.[max - 1] = c
    a <> b


let Day2b () =
    let res =
        input
        |> List.filter (fun x ->
            let a, b, c, d = splitRow x
            checkPwd2b a b c d)

    printfn "Day2b: %d" res.Length
