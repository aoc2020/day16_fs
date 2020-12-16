open System

open day16.BaseTypes
open day16.Input
                 
let completelyInvalid (fields:Field[]) (ticket:Ticket) : bool =
    let invalid (value:uint64) : bool =
        fields |> Seq.filter (fun (field:Field) -> field.isValid value) |> Seq.isEmpty
    ticket |> Seq.filter (invalid) |> Seq.isEmpty

let completelyInvalidValues (fields:Field[]) (ticket:Ticket) : uint64[] =
    let invalid (value:uint64) : bool =
        fields |> Seq.filter (fun (field:Field) -> field.isValid value) |> Seq.isEmpty
    ticket |> Seq.filter (invalid) |> Seq.toArray 

let task1 fields nearby = 
    let invalidFields = nearby |> Seq.map (completelyInvalidValues fields) |> Seq.concat
    let answer1 = invalidFields |> Seq.sum 
    printfn "invalidFields = %A" invalidFields
    printfn "Answer 1 = %d" answer1 

let withoutCompletelyInvalid (ticketData:TicketData) :TicketData =
    let validNearby = ticketData.Nearby |> Seq.filter (completelyInvalid ticketData.Fields) |> Seq.toArray 
    ticketData.replaceNearby validNearby     

let task2 (ticketData:TicketData) =
    let data1 = withoutCompletelyInvalid ticketData  
    printf "$$$ %A" data1 

[<EntryPoint>]
let main argv =
    let input = "/Users/xeno/projects/aoc2020/day16_fs/input2.txt"
    let inputData = readSplitInput input    
    let fields = inputData.Fields |> Seq.map (Field) |> Seq.toArray 
    let nearby = inputData.Nearby
    let data:TicketData = TicketData(fields,inputData.Yours,inputData.Nearby)
    task1 fields nearby
    task2 data 
    
    0 // return an integer exit code
