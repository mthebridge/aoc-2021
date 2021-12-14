module day14

let expectedTest = (1588L, 2188189693529L)

let parseRule (line: string) =
    let spl = line.Split " -> "
    (spl.[0], spl.[1].Chars 0)


let incrOrInit (x: Option<int64>) =
    match x with
    | Some (v) -> Some(v + 1L)
    | None -> Some(0L)

let runStep (chain: list<char>) (counts: Map<char, int64>) (rules: Map<string, char>) last =
    let mutable newCounts = counts

    let newChain =
        [ for (a, b) in chain |> List.pairwise do
            let inserted = ($"{a}{b}" |> rules.TryFind).Value
            // Return the first and inserted character
            newCounts <- (newCounts |> Map.change inserted incrOrInit)
            yield! [ a; inserted ]
          yield last ]

    (newCounts, newChain)

let getScore rules chain steps =
    // Always keep the last element separately (as it never gets a pair)
    let last = chain |> Seq.last
    // Keep track of counts as we go.
    let startCounts =
        chain
        |> Seq.countBy id
        |> Map.ofSeq
        |> Map.map (fun k v -> int64 v)

    let (counts, chain) =
        ((startCounts, chain), seq { 1 .. steps })
        ||> Seq.fold (fun (counts, chain) i -> runStep chain counts rules last)

    printfn $"Chain is now: {chain |> Seq.length} long: {counts}"

    let counts =
        counts |> Map.values |> Seq.sortDescending

    (counts |> Seq.head) - (counts |> Seq.last)

let run (input: string []) =

    let chain = input.[0] |> Seq.toList

    let rules =
        input.[2..] |> Array.map parseRule |> Map.ofArray

    let part1 = getScore rules chain 10
    let part2 = getScore rules chain 40
    int64 part1, int64 part2
