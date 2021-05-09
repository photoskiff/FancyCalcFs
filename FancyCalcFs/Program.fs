#nowarn "40"
open System
open System.Collections.Generic
open System.IO

let memoize fn =
  let cache = new System.Collections.Generic.Dictionary<_,_>()
  (fun x ->
    match cache.TryGetValue x with
    | true, v -> v
    | false, _ -> let v = fn (x)
                  //printfn "func called"
                  cache.Add(x,v)
                  v)

type Token = Add = 0 | Mult = 1 | Value = 2
type Operation = {Op:Token; Labels: int[]}

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let getOperations (filePath:string) = 
    let ops = readLines filePath
    let result = ops |> Seq.map (fun x -> 
        let values = x.Split ' ' |> Seq.map(fun c -> c.Trim().TrimEnd ':') |> Seq.toList
        let labels = values |> Seq.skip 2 |> Seq.map (fun l -> l |> int) |> Seq.toArray
        let value = {Op = Enum.Parse<Token>(values.[1]); Labels = labels }
        let key = values.[0] |> int
        key, value) |> Seq.toArray |> dict
    result;

let fancyCalc  (operations:IDictionary<int, Operation>)  = 
    let rec Compute:(int->int64) = memoize(fun label -> 
        let _, op = operations.TryGetValue label
        match op.Op with
        | Token.Value -> int64 op.Labels.[0]
        | Token.Add -> Array.fold (fun x y -> x + Compute(y)) 0L op.Labels
        | _ -> Array.fold (fun x y -> x * Compute(y)) 1L op.Labels
    )
    let first = operations.Keys |> Seq.take 1 |> Seq.toList
    Compute first.[0]

[<EntryPoint>]
let main argv =
    let ops = getOperations "input.txt" 
//    let input = testDict
    let result = fancyCalc ops
    printfn $"{result}"
    0 


