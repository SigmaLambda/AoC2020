module Day8

let testinput =
    System.IO.File.ReadAllLines ".//input//Day8Test.txt"
    |> Array.toList


let parseInput (input: string list) =
    input
    |> List.map (fun x ->
        x.Split(" ")
        |> fun x -> (x.[0], x.[1]))
    |> List.map (fun (op, num) -> op, (int num))


let makeMemory ins jump = String.concat " " [ ins; (string jump) ]
let isOldMemory newMem memory = List.contains newMem memory



let rec Assembler acc ip (input: (string * int) list) memory =
    //printfn "OldMem: %A" memory
    let ins, jump = input.[ip]
    let newMem = ip
    //printfn "NewMem: %d" newMem
    //printfn "ins: %s %d \tacc: %d ip: %d" ins jump acc ip

    if isOldMemory newMem memory then
        false, acc
    else if (ip = input.Length - 1) then
        true, (acc + jump)
    else
        match ins with
        | "acc" ->
            let newAcc = acc + jump
            let newIp = ip + 1
            Assembler newAcc newIp input (newMem :: memory)
        | "jmp" ->
            let newAcc = acc
            let newIp = ip + jump

            Assembler newAcc newIp input (newMem :: memory)
        | "nop" -> Assembler acc (ip + 1) input (newMem :: memory)
        | _ -> failwith "AsemblyError"

let Day8Test () =
    let acc =
        Assembler 0 0 (parseInput testinput) List.Empty

    printfn "Day8 Test: %A" acc


let Day8 () =
    let input =
        parseInput
            (System.IO.File.ReadAllLines ".//input//Day8.txt"
             |> Array.toList)

    let acc = Assembler 0 0 input List.Empty

    printfn "Day8 Test: %A" acc


let GetJmpPos input =
    input
    |> List.mapi (fun i (ins: string) -> if (ins.Contains("jmp")) then Some i else None)
    |> List.choose id


let GetNopPos input =
    input
    |> List.mapi (fun i (ins: string) -> if (ins.Contains("nop")) then Some i else None)
    |> List.choose id

let SwitchParsedJmpToNop ip switchTo input =
    List.mapi (fun i ins ->
        if i = ip then
            let ins, jmp = ins
            (switchTo, jmp)
        else
            ins) input

let SwitchParsedNopToJmp ip switchTo input =
    List.mapi (fun i ins ->
        if i = ip then
            let ins, jmp = ins
            (switchTo, jmp)
        else
            ins) input


let Day8BTest () =
    let input =
        parseInput
            (System.IO.File.ReadAllLines ".//input//Day8.txt"
             |> Array.toList)

    let jmpPos = GetJmpPos testinput
    let nopPos = GetNopPos testinput
    
    List.map (fun pos ->
        let newInput =
            parseInput testinput
            |> SwitchParsedJmpToNop pos "nop"

        let acc = Assembler 0 0 newInput List.Empty

        printfn "Day8B Test: %A" acc) jmpPos
    |> ignore

    List.map (fun pos ->
        let newInput =
            parseInput testinput
            |> SwitchParsedNopToJmp pos "jmp"

        let acc = Assembler 0 0 newInput List.Empty

        printfn "Day8B Test: %A" acc) nopPos
    |> ignore

let Day8B () =
    let inputB =
        System.IO.File.ReadAllLines ".//input//Day8.txt"
        |> Array.toList

    let jmpPos = GetJmpPos inputB
    let nopPos = GetNopPos inputB

    List.map (fun pos ->
        let newInput =
            parseInput inputB
            |> SwitchParsedJmpToNop pos "nop"

        let acc = Assembler 0 0 newInput List.Empty

        printfn "Day8B: %A" acc) jmpPos
    |> ignore

    List.map (fun pos ->
        let newInput =
            parseInput inputB
            |> SwitchParsedNopToJmp pos "jmp"

        let acc = Assembler 0 0 newInput List.Empty

        printfn "Day8B: %A" acc) nopPos
    |> ignore
