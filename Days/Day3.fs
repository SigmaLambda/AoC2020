module Day3

let testinput =
    [| "..##......."
       "#...#...#.." //1
       ".#....#..#."
       "..#.#...#.#"
       ".#...##..#." //4
       "..#.##....."
       ".#.#.#....#"
       ".#........#"
       "#.##...#..." //8
       "#...##....#"
       ".#..#...#.#" |]

let testmap =
    testinput |> Array.map (fun x -> x.ToCharArray())

//10 w 11 d
let rec traverse (map: char [] []) cR cC maxR maxC treeCount moveR moveC =

    //printfn "R %d C %d" cR cC
    //printfn "Row: %A" map.[cR]
    if cR >= maxR then
        treeCount
    else if cC >= maxC then
        let newCC = cC - maxC
        //printfn "newCC %d" newCC
        let newTree = if map.[cR].[newCC] = '#' then 1 else 0
        //printfn "newtree: %d" newTree
        traverse map (cR + moveR) (newCC + moveC) maxR maxC (treeCount + newTree) moveR moveC
    else
        let newTree = if map.[cR].[cC] = '#' then 1 else 0
        //printfn "newtree: %d" newTree
        traverse map (cR + moveR) (cC + moveC) maxR maxC (treeCount + newTree) moveR moveC

let Runtest3a () =

    let res = traverse testmap 1 3 (11) (11) 0 1 3
    printfn "Day3 test: %d" res


let aMap =
    System.IO.File.ReadAllLines "./Input/Day3.txt"
    |> Array.map (fun x -> x.ToCharArray())


let Run3a () =

    let resA = traverse aMap 1 3 323 31 0 1 3
    printfn "Day3a: %d" (resA)

let Run3b () =
    //Right 1, down 1.
    let res1 = int64 (traverse aMap 1 1 323 31 0 1 1)
    printfn "1: %d" (res1)

    //Right 3, down 1. (This is the slope you already checked.)
    let res2 = int64 (traverse aMap 1 3 323 31 0 1 3)
    printfn "2: %d" (res2)

    //Right 5, down 1.
    let res3 = int64 (traverse aMap 1 5 323 31 0 1 5)
    printfn "3: %d" (res3)

    //Right 7, down 1.
    let res4 = int64 (traverse aMap 1 7 323 31 0 1 7)
    printfn "4: %d" (res4)

    //Right 1, down 2.
    let res5 = int64 (traverse aMap 2 1 323 31 0 2 1)
    printfn "5: %d" (res5)

    printfn "Day3a: %d" (res1 * res2 * res3 * res4 * res5)
