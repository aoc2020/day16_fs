module day16.Input

open System
open System.IO

let input (filePath: String) = seq {
    use sr = new StreamReader(filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let splitByBlank (input:seq<String>):String[][] =
    let init = [[]]
    let acc = input |> Seq.fold (fun (acc:List<List<String>>) (line:String) ->
                            if line = "" then
                                [] :: acc
                            else
                                (line :: acc.Head) :: acc.Tail                                
                       ) init
    let revArray list = list |> Seq.rev |> Seq.toArray 
    acc |> Seq.map revArray |> revArray   

type InputField (name: String, range1:uint64*uint64,range2:uint64*uint64) as self =
    override this.ToString () = sprintf "InputField(%s %A %A)"name range1 range2
    new (input:String) =
        let s = input.Split ':'
        let r = s.[1].Split "or" 
        let name = s.[0]
        let range1 = r.[0].Split '-' |> Seq.map uint64 |> Seq.toArray
        let range2 = r.[1].Split '-' |> Seq.map uint64 |> Seq.toArray
        InputField (name, (range1.[0],range1.[1]), (range2.[0],range2.[1]))
    member this.Name = name
    member this.Range1 = range1
    member this.Range2 = range2
  
type InputData (fields:InputField[], yours:uint64[], nearby:uint64[][]) as self =
    override this.ToString () = sprintf "InputData fields:%A yours:%A nearby:%A" fields yours nearby
    member this.Fields = fields
    member this.Yours = yours
    member this.Nearby = nearby 
    
let readSplitInput (filePath:String) : InputData = 
    let lines = input filePath |> Seq.toArray
    let split = splitByBlank lines
    let fields = split.[0] |> Seq.map (InputField) |> Seq.toArray 
    let yours = split.[1].[1].Split ',' |> Seq.map uint64 |> Seq.toArray 
    let nearby = split.[2].[1..]
                 |> Seq.map (fun (s:String) -> s.Split ',' |> Seq.map uint64 |> Seq.toArray )
                 |> Seq.toArray 
    printfn "%A" lines
    printfn "Fields: %A" fields 
    printfn "Yours: %A" yours
    printfn "Nearby: %A" nearby
    InputData(fields,yours,nearby)
