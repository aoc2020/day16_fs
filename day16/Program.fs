open System

open day16.BaseTypes
open day16.Input

        
                
let completelyInvalidValues (fields:Field[]) (ticket:uint64[]) : bool =
    let invalid (value:uint64) : bool =
        fields |> Seq.filter (fun (field:Field) -> field.isValid value) |> Seq.isEmpty
    ticket |> Seq.filter (invalid) |> Seq.isEmpty

let completelyInvalid (fields:Field[]) (ticket:uint64[]) : uint64[] =
    let invalid (value:uint64) : bool =
        fields |> Seq.filter (fun (field:Field) -> field.isValid value) |> Seq.isEmpty
    ticket |> Seq.filter (invalid) |> Seq.toArray 

let task1 fields nearby = 
    let invalidFields = nearby |> Seq.map (completelyInvalid fields) |> Seq.concat
    let answer1 = invalidFields |> Seq.sum 
    printfn "invalidFields = %A" invalidFields
    printfn "Answer 1 = %d" answer1 

[<EntryPoint>]
let main argv =
    let input = "/Users/xeno/projects/aoc2020/day16_fs/input.txt"
    let inputData = readSplitInput input    
    let fields = inputData.Fields |> Seq.map (Field) |> Seq.toArray 
    let nearby = inputData.Nearby
    task1 fields nearby 
    
    0 // return an integer exit code
