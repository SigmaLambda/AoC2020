module Day4

open System.Text.RegularExpressions

let testinput =
    System.IO.File.ReadAllLines ".//input//Day4Test.txt"
    |> Array.map (fun s -> s + ";")
    |> Array.toList



let rec groupData (temp: string list) (lines: string list) (raw: string list) =
    match raw with
    | [] ->
        let newLine = List.fold (+) "" temp
        (newLine :: lines)
    | hd :: tl ->
        if hd = ";" then
            let newLine = List.fold (+) "" temp
            groupData List.empty (newLine :: lines) tl
        else
            groupData (hd :: temp) lines tl

let SplitToPair (s: string) =
    //printfn "%s" s
    let split = s.Split(':')
    split.[0], split.[1]

let parseLines (line: string) =
    // printfn "%s" line
    let newLine = line.[..line.Length - 2]
    // printfn "%s" newLine

    let words = newLine.Split(' ', ';')
    let pair = Array.map SplitToPair words
    Map.ofArray pair

let CheckValid (p: Map<string, string>) =
    if p.Count = 8 then
        printfn "8: %A" p
        true
    else if p.Count = 7 then
        printf "7: %A" p
        match p.TryFind "cid" with
        | Some s ->
            printfn "\tfalse"
            false
        | None ->
            printfn "\ttrue"
            true
    else
        printfn "else: %A \t%d " p p.Count
        false

let RunTest4 () =
    let res =
        testinput
        |> groupData [] []
        |> List.map parseLines
        |> List.map CheckValid
        |> List.filter id

    printfn "%d" res.Length

let input =
    System.IO.File.ReadAllLines ".//input//Day4.txt"
    |> Array.map (fun s -> s + ";")
    |> Array.toList


let Day4a () =
    let res =
        input
        |> groupData [] []
        |> List.map parseLines
        |> List.map CheckValid
        |> List.filter id

    printfn "%d" res.Length


//byr (Birth Year) - four digits; at least 1920 and at most 2002.
let byrCheck b =
    let c = int b
    (c >= 1920 && c <= 2002)

//iyr (Issue Year) - four digits; at least 2010 and at most 2020.
let iyrCheck i =
    let j = int i
    j >= 2010 && j <= 2020

//eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
let eyrCheck e =
    let f = int e
    f >= 2020 && f <= 2030

//hgt (Height) - a number followed by either cm or in:
//If cm, the number must be at least 150 and at most 193.
//If in, the number must be at least 59 and at most 76.
let hgtCheck (h: string) =
    if Regex.IsMatch(h, "\d+in") then
        let height = h.Replace("in", "") |> int
        height >= 59 && height <= 76
    else if Regex.IsMatch(h, "\d+cm") then
        let height = h.Replace("cm", "") |> int
        height >= 150 && height <= 193
    else
        false



//hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
let hclCheck h = Regex.IsMatch(h, "#\w{6}")

//ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
let eclCheck e =
    e = "amb"
    || e = "blu"
    || e = "brn"
    || e = "gry"
    || e = "grn"
    || e = "hzl"
    || e = "oth"

//pid (Passport ID) - a nine-digit number, including leading zeroes.
let pidCheck p = Regex.IsMatch(p, "\d{9}")

//cid (Country ID) - ignored, missing or not.
let cidCheck c = true

let hardCheck (p: Map<string, string>) =
    byrCheck p.["byr"]
    && iyrCheck p.["iyr"]
    && eyrCheck p.["eyr"]
    && hgtCheck p.["hgt"]
    && hclCheck p.["hcl"]
    && eclCheck p.["ecl"]
    && pidCheck p.["pid"]

let CheckValidB (p: Map<string, string>) =
    if p.Count = 8 then
        //printfn "8: %A" p
        hardCheck p
    else if p.Count = 7 then
        printf "7: %A" p
        match p.TryFind "cid" with
        | Some s ->
            printfn "\tfalse"
            false
        | None ->
            printfn "\ttrue"
            hardCheck p
    else
        printfn "else: %A \t%d " p p.Count
        false

let Day4bTest () = 
    //eyr:1972 cid:100
    //hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
    //
    //iyr:2019
    //hcl:#602927 eyr:1967 hgt:170cm
    //ecl:grn pid:012533040 byr:1946
    //
    //hcl:dab227 iyr:2012
    //ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
    //
    //hgt:59cm ecl:zzz
    //eyr:2038 hcl:74454a iyr:2023
    //pid:3556412378 byr:2007

let Day4b () =
    let res =
        input
        |> groupData [] []
        |> List.map parseLines
        |> List.map CheckValidB
        |> List.filter id

    printfn "%d" res.Length
