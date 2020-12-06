﻿// Learn more about F# at http://fsharp.org

open System


[<EntryPoint>]
let main argv =
    
    Day1.RunDay1 () |> ignore    
    Day2.Day2a () |> ignore
    Day2.Day2b () |> ignore
    //Day3.Runtest3a () |> ignore
    //Day3.Run3a () |> ignore
    //Day3.Run3b () |> ignore
    Day4.Day4a () |> ignore
    Day4.RunTest4 () |> ignore
    //Day4.Day4bTest () |> ignore
    //Day4.Day4b () |> ignore
    Day5.TestA () |> ignore
    Day5.Day5A () |> ignore
    Day5.Day5B () |> ignore
    Day6.Day6 () |> ignore
    Day6.Day6b () |> ignore

    
    0 // return an integer exit code
