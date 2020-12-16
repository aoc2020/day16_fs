
open System
open System.IO

open day16
open day16.Input

type Range (startAt:uint64,endAt:uint64) as self =
    override this.ToString() = sprintf "%d-%d" startAt endAt
    member this.contains (value:uint64) : bool = value >= startAt && value <= endAt 

type Field (name: String, range1:Range,range2:Range) as self =
    override this.ToString () = sprintf "Field(%A %A or %A)" name range1 range2
    new (f:InputField) =
        let range1 = Range(fst f.Range1,snd f.Range1)
        let range2 = Range(fst f.Range2,snd f.Range2)
        Field(f.Name,range1,range2)
    member this.isValid (value:uint64) : bool =
        range1.contains value || range2.contains value          
                
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
