module Day7

type Bag =
    { Quant: int
      Desc: string
      Color: string
      Key: string }


let CreateKey (s: string) =
    let newS = s.Trim().Split(" ")
    String.concat " " [ newS.[0]; newS.[1] ]



let CreateBag (s: string []) =
    // printfn "%A" s
    { Quant = (int s.[0])
      Desc = s.[1]
      Color = s.[2]
      Key = String.concat " " [ s.[1]; s.[2] ] }


let CreateBags (cs: string []) =
    let strings =
        cs |> Array.map (fun c -> c.Trim().Split(" "))

    if strings.[0].[0] = "no" then Array.empty else (Array.map CreateBag strings)


let CreateInput path =
    System.IO.File.ReadAllLines path
    |> Array.map (fun l -> l.Split(" bags contain "))
    |> Array.map (fun l -> l.[0], l.[1].Split(","))
    |> Array.map (fun (s, ss) -> (CreateKey s), Array.toList (CreateBags ss))
    |> Array.toList




let BagCanContain target key (values: Bag list) dict =
    if values.Length > 0 then
        let canContain =
            values
            |> List.map (fun v -> Some(v.Key = target || List.contains v.Key dict))
            |> List.choose id
            |> List.reduce (||)

        match canContain with
        | false -> None
        | true -> Some key
    else
        None

let rec LoopAllBags target (allbags: (string * Bag list) list) dict =

    let (res: string list) =
        allbags
        |> List.map (fun (key: string, vs: Bag list) -> BagCanContain target key vs dict)
        |> List.choose id
        |> List.distinct

    let distinct = res @ dict |> List.distinct
    if distinct.Length > dict.Length then LoopAllBags target allbags distinct else dict



// let rec AddBagsInShinyBag (targetValues: Bag list) (allbags: Map<string, Bag list>) acc =

//     targetValues
//     |> Map.map (fun v ->
//         v.Quant * AddBagsInShinyBag v allbags)


// let da7B () =




let target = "shiny gold"

let Day7Run path =
    let res =
        let input = (CreateInput path)
        LoopAllBags target input List.Empty


    printfn "Day7a %A" res |> ignore
    res.Length

let Day7Test () =
    let res = Day7Run ".//Input//Day7Test.txt"
    //    Day7Run "..//..//..//Input//Day7Test.txt"

    printfn "Day7 Test: %d" res

Day7Test()

let Day7A () =
    //let res = Day7Run "..//..//..//Input//Day7.txt"
    let res = Day7Run ".//Input//Day7.txt"
    printfn "Day7 A: %d" res

Day7A()
