module day6

let runDay (input: string[]) =

    // All that matters is how many fish there are of each value.
    let fish =
        input.[0].Split ","
        |> Array.map int
        |> Array.countBy (fun f -> f)
        |> Array.map (fun (f, count) -> (f, uint64 count))
        |> Array.toList

    let runstep fish =
        let (newFish, newCount: uint64) =
            (0UL, fish)
            ||> List.mapFold (fun newCount (fishVal, count) ->
                if fishVal = 0 then
                    ((6, count), newCount + count)
                else
                    ((fishVal - 1, count), newCount))

        if newCount > 0UL then
            (8, newCount) :: newFish
        else
            newFish

    let rec runSimulation fish steps =
        match steps with
        | 0 -> fish
        | count -> runSimulation (runstep fish) (count - 1)

    let countFishAfterSteps fish steps =
        runSimulation fish steps
        |> List.sumBy (fun (_, count) -> count)

    let part1 = countFishAfterSteps fish 80
    printfn $"part1: {part1}"

    let part2 = countFishAfterSteps fish 256
    printfn $"part2: {part2}"
