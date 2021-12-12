module day6

let expectedTest = (5934L, 26984457539L)

let run (input: string[]) =

    // All that matters is how many fish there are of each value.
    let fish =
        input.[0].Split ","
        |> Array.map int
        |> Array.countBy (fun f -> f)
        |> Array.map (fun (f, count) -> (f, int64 count))
        |> Array.toList

    let runstep fish =
        let (newFish, newCount: int64) =
            (0L, fish)
            ||> List.mapFold (fun newCount (fishVal, count) ->
                if fishVal = 0 then
                    ((6, count), newCount + count)
                else
                    ((fishVal - 1, count), newCount))

        if newCount > 0L then
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
    let part2 = countFishAfterSteps fish 256
    part1, part2
