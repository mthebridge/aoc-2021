module day14

let expectedTest = (1588L, 2188189693529L)


let parseRule (line: string) =
    let spl = line.Split " -> "
    let pair = spl.[0]
    ((pair.Chars 0, pair.Chars 1), spl.[1].Chars 0)

type PendingPair = { a: char; b: char; steps: int }
type Counts = Map<char, int64>

// Cache maps pairs to sets of depth and counts
type Cache = Map<PendingPair, Counts>

// A single global cache for speed
let mutable gCache: Cache = Map.empty

let incrOrInit (x: Option<int64>) =
    match x with
    | Some (v) -> Some(v + 1L)
    | None -> Some(0L)

let mergeCounts (a: Counts) (b: Counts) : Counts =
    let allKeys = seq { yield! a.Keys; yield! b.Keys}
    let getValOrZero (map: Counts) k =
        match map.TryFind k with
        | Some (v) -> v
        | None -> 0L

    allKeys
    |> Seq.map (fun k -> (k, getValOrZero a k + getValOrZero b k))
    |> Map.ofSeq

let getScore (rules: Map<char*char, char>) (chain: seq<char>) steps =
    // let mutable cache: Cache = Map.empty
    let rec getPairCounts pair =
        match pair |> gCache.TryFind with
        |Some (hit) -> hit
        |None ->
            let inserted = ((pair.a, pair.b) |> rules.TryFind).Value
            let thisCount = seq { (inserted, 1L) } |> Map.ofSeq
            let newCounts: Counts =
                if pair.steps = 1 then
                    thisCount
                else
                   // Still more steps to go.
                   // Work out the inner values, and merge them with this level.
                   (thisCount,  (getPairCounts { a = pair.a;
                            b = inserted;
                            steps = pair.steps - 1 },
                        getPairCounts { a = inserted;
                            b = pair.b;
                            steps = pair.steps - 1 }
                        )
                        ||> mergeCounts
                    ) ||> mergeCounts
            // Cache and return merged new values
            gCache <- (gCache |> Map.add pair newCounts)
            newCounts

    let pairs =
        chain
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> { a = a; b = b; steps = steps })
        |> Seq.toList

    // Keep track of counts as we go.
    let counts: Counts =
        rules
        |> Map.values
        |> Seq.map (fun v -> (v, (chain |> Seq.filter(fun x -> x = v)) |> Seq.length|> int64))
        |> Map.ofSeq

    printfn $"Initial counts: {counts}"
    let counts =
        (counts, pairs)
        ||> List.fold (fun counts pair ->
            (counts, getPairCounts pair) ||> mergeCounts
        )

    printfn $"Final counts: {counts}"
    let counts =
        counts |> Map.values |> Seq.sortDescending

    (counts |> Seq.head) - (counts |> Seq.last)

let run (input: string []) =

    let chain = input.[0]

    let rules =
        input.[2..] |> Array.map parseRule |> Map.ofArray

    let part1 = getScore rules chain 10
    let part2 = getScore rules chain 40

    int64 part1, int64 part2
