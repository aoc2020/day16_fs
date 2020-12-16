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
    let isValid = field.isValid value
    isValid

let invalidAtPos field ticket pos =
    validAtPos field ticket pos |> not

let someInvalidAtPos field (tickets:Ticket[]) pos =   
    tickets |> Seq.exists (fun tick -> invalidAtPos field tick pos)

let validPosition field (tickets:Ticket[]) pos =
    let someInvalid = someInvalidAtPos field tickets pos
    someInvalid |> not 
    
let candidatePositions (field:Field) (tickets:Ticket[]) (range:int[]) : int[] =
    let pos = range |> Seq.filter (fun (i:int) -> validPosition field tickets i) |> Seq.toArray 
    pos 
    
let candidatesPerPos (ticketData:TicketData) (range:int[]) : int[][] =
    let nearby = ticketData.Nearby
    let fields = ticketData.Fields    
    let candidates = fields |> Seq.map (fun (f:Field) -> candidatePositions f nearby range)
    candidates |> Seq.map Seq.toArray |> Seq.toArray 

let mapToMyValues (ticketData:TicketData) (determined:Option<int>[]) : (Field*uint64)[] =
    let myValue (field:Field) (pos:Option<int>) = (field,ticketData.Yours.[pos.Value])
    Seq.map2 myValue ticketData.Fields determined |> Seq.toArray 

let calcResult (fieldValues: (Field*uint64)[]) : uint64 =
    let multiply a b = a * b
    let relevant (fieldValue:Field*uint64) : bool =
        let field = fst fieldValue
        field.Name.StartsWith "departure"
    let values = fieldValues |> Seq.filter relevant |> Seq.map snd
    let result = values |> Seq.fold multiply 1UL
    result 

let task2 (ticketData:TicketData) =
    let data1 = withoutCompletelyInvalid ticketData  
    let range = [0..data1.Yours.Length-1] |> Seq.toArray   
    let candidates: int[][] = candidatesPerPos data1 range
    let identified : Option<int>[] = range |> Seq.map (fun f -> None) |> Seq.toArray 
    let determined = identifyAll candidates identified
    let myValues = mapToMyValues ticketData determined
    let answer2 = calcResult myValues
    printfn "Answer 2: %A" answer2
    
    
[<EntryPoint>]
let main argv =
    let input = "/Users/xeno/projects/aoc2020/day16_fs/input.txt"
    let inputData = readSplitInput input    
    let fields = inputData.Fields |> Seq.map (Field) |> Seq.toArray 
    let nearby = inputData.Nearby
    let data:TicketData = TicketData(fields,inputData.Yours,inputData.Nearby)
    task1 fields nearby
    task2 data 
    
    0 // return an integer exit code
