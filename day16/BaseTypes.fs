module day16.BaseTypes

open System
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

