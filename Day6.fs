module day6

let runDay =
    let input =
        System.IO.File.ReadAllLines(@"day6input.txt")

    // All that matters is how many fish there are of each value.
    let fish = input[0].Split ","  |> Array.map int |> Array.toList

    let runstep fish =
        let (newFish, newCount) = (0, fish) ||> List.mapFold (fun newCount f ->
            if f = 0 then
                (6, newCount + 1)
            else
                (f - 1, newCount)
        )
        // do printfn $"{newCount} new fish today"
        List.append newFish [ for _ in 1 .. newCount -> 8 ]

    let rec runSimulation fish steps =
        match steps with
        |0 -> fish
        |count -> printfn $"Steps left: {steps}"; runSimulation (runstep fish) (count - 1)

    let part1 = runSimulation fish 80 |> List.length
    printfn $"part1: {part1}"
    let part2 = runSimulation fish 256 |> List.length
    printfn $"part2: {part2}"
