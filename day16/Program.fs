open System

open day16.BaseTypes
open day16.Input
open day16.Identify 
                 
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

let validAtPos (field:Field) (ticket:Ticket) (pos:int) =
    let value = ticket.[pos]
    field.isValid value

let invalidAtPos field ticket pos = validAtPos field ticket pos |> not

let someInvalidAtPos field (tickets:Ticket[]) pos =
    tickets |> Seq.exists (fun tick -> invalidAtPos field tick pos)

let validPosition field (tickets:Ticket[]) pos =
    let someInvalid = someInvalidAtPos field tickets pos
    someInvalid |> not 
    
let candidatePositions (field:Field) (tickets:Ticket[]) (range:int[]) =
    range |> Seq.filter (fun (i:int) -> validPosition field tickets i)     
    
let candidatesPerPos (ticketData:TicketData) (range:int[]) : int[][] =
    let nearby = ticketData.Nearby
    let fields = ticketData.Fields    
    let candidates = fields |> Seq.map (fun (f:Field) -> candidatePositions f nearby range)
    candidates |> Seq.map Seq.toArray |> Seq.toArray 

let task2 (ticketData:TicketData) =
    let data1 = withoutCompletelyInvalid ticketData  
    printfn "$$$ %A" data1
    let range = [0..data1.Yours.Length-1] |> Seq.toArray   
    printfn "Range: %A" range
    let candidates: int[][] = candidatesPerPos ticketData range
    let identified : Option<int>[] = range |> Seq.map (fun f -> None) |> Seq.toArray 
    printfn "Candidates: %A" candidates
    printfn "Identified: %A" identified
    let determined = identifyAll candidates identified
    printfn "Determined: %A" determined 
    
[<EntryPoint>]
let main argv =
    let input = "/Users/xeno/projects/aoc2020/day16_fs/input3.txt"
    let inputData = readSplitInput input    
    let fields = inputData.Fields |> Seq.map (Field) |> Seq.toArray 
    let nearby = inputData.Nearby
    let data:TicketData = TicketData(fields,inputData.Yours,inputData.Nearby)
    task1 fields nearby
    task2 data 
    
    0 // return an integer exit code
