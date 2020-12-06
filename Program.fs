// Learn more about F# at http://fsharp.org

open System


[<EntryPoint>]
let main argv =
    
    Day1.RunDay1 () |> ignore    
    Day2.Day2a () |> ignore
    Day2.Day2b () |> ignore
    Day4.RunTest4 () |> ignore
    Day4.Day4a () |> ignore
    Day4.Day4bTest () |> ignore
    Day4.Day4b () |> ignore


    
    printfn "Hello World from F#!"
    0 // return an integer exit code
