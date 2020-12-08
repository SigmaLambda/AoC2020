module Day7

type Bag =
    { Quant: int
      Desc: string
      Color: string }


let CreateContainerBag (s: string) =
    let newS = s.Trim().Split(" ")
    { Quant = 99
      Desc = newS.[0]
      Color = newS.[1] }

let CreateBag (s: string []) =
    { Quant = (int s.[0])
      Desc = s.[1]
      Color = s.[2] }



let CreateBags (cs: string []) =
    let strings =
        cs |> Array.map (fun c -> c.Trim().Split(" "))

    if strings.[0].[0] = "no" then Array.empty else (Array.map CreateBag strings)




let testinput =
    System.IO.File.ReadAllLines ".//Input//Day7Test.txt"
    |> Array.map (fun l -> l.Split(" bags contain "))
    |> Array.map (fun l -> l.[0], l.[1].Split(","))
    |> Array.map (fun (s, ss) -> (CreateContainerBag s), Array.toList (CreateBags ss))
    |> List.ofArray



let target = CreateBag [| "99"; "shiny"; "gold" |]

let rec Search (t: Bag) (bs: Bag list) (m: (Bag * Bag list) list): int =

    List.sumBy (fun b -> if b = t then 1 else 0) bs

    + List.sumBy (fun b ->
        let next =
            { Quant = b.Quant
              Desc = b.Desc
              Color = b.Color }

        printfn "%A" next
        match List.tryFind (fun (b, bs) -> b = next) m with
        | None -> 0
        | Some bbs -> Search t (snd bbs) m) bs


let Day7Test () =
    let res =
        testinput
        |> List.sumBy (fun (t, ts) -> Search target ts testinput)

    printfn "Day7a %d" res
