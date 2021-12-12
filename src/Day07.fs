module day7

let expectedTest = (37L, 168L)

let run (input: string []) =

    let crabs =
        input.[0].Split "," |> Array.map int |> Array.sort

    // Find N such that  (sum of all crab values - N) is minimised.
    let max = crabs.Length

    let getFuelCost pos crabs =
        // For each crab, fuel cost is distance from pos
        crabs
        |> Array.map (fun c -> abs (c - pos))
        |> Array.sum

    let getFuelCost2 pos crabs =
        // For each crab, fuel cost is the "distance from pos"th triangualr number
        crabs
        |> Array.map (fun c ->
            let dist = abs (c - pos)
            (dist * (dist + 1) / 2))
        |> Array.sum

    let validPositions =
        seq { crabs.[0] .. crabs.[crabs.Length - 1] }

    let getMinimumFuel crabs getCost =
        (100000000, validPositions)
        ||> Seq.fold (fun bestFuel c -> min (getCost c crabs) bestFuel)

    let minFuel1 = getMinimumFuel crabs getFuelCost
    let minFuel2 = getMinimumFuel crabs getFuelCost2

    int64 minFuel1, int64 minFuel2
