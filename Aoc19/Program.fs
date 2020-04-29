// Learn more about F# at http://fsharp.org

open System.IO

let logFirst output =    
    output
    |> Array.iteri (fun i element ->
        match i with
        | 0 -> printfn "%i" element
        | _ -> ())

let convertAndAdjustInputValues i (raw: string) =
    match i with
    | 1 -> 12
    | 2 -> 2
    | _ -> int raw

let toAdjustedProgram (input: string) =
    input.Split ','
    |> Array.mapi convertAndAdjustInputValues
    
let solveInput input =
    input
    |> toAdjustedProgram
    |> IntcodeComputer.run
    |> logFirst

[<EntryPoint>]
let main argv =
    File.ReadAllLines (Path.Combine [| Directory.GetCurrentDirectory (); "Inputs/day2.txt" |])
        |> Seq.iter solveInput
    0 // return an integer exit code
