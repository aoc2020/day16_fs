module day16.Identify

type IntSet = Set<int>

let getKnown (identified:Option<int>[]) =
    let addIfSome (s:IntSet) (oi:Option<int>) =
        match oi with
        | None -> s
        | Some i -> s.Add i
    identified |> Seq.fold addIfSome Set.empty

let removeKnown (candidates:int[][]) (known:IntSet) =
    let isUnknown (i:int) = known.Contains i |> not 
    let remove (candidates:int[]) =
        candidates |> Seq.filter isUnknown |> Seq.toArray
    let cleaned = candidates |> Seq.map remove |> Seq.toArray
    cleaned

let identify (id:Option<int>) (candidates:int[]) =
    match id with
    | Some _ -> id
    | None ->
        if candidates.Length = 1
        then Some candidates.[0]
        else None
        
let allIdentified (identified:Option<int>[]) : bool =
    identified |> Seq.filter (fun (i:Option<int>) -> i.IsNone ) |> Seq.isEmpty 

let rec identifyAll (candidates:int[][]) (identified:Option<int>[]): Option<int>[] =
    let newlyIdentified= Seq.map2 identify identified candidates |> Seq.toArray 
    let known = getKnown newlyIdentified 
    let cleaned = removeKnown candidates known 
    printfn "known: %A cleaned: %A" known cleaned
    if allIdentified newlyIdentified
    then newlyIdentified
    else identifyAll cleaned newlyIdentified 
    