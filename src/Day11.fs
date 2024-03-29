module day11

let expectedTest = (1656L, 195L)

let GRIDSIZE = 10
let MAX_ENERGY = 9
type Grid = int [] []

let printGrid grid =
    grid
    |> Array.iter (fun row ->
        row |> Array.iter (fun e -> printf $"{e}")
        printfn "")

let getNeighbours x y =
    seq {
        for x' in max 0 (x - 1) .. min (GRIDSIZE - 1) (x + 1) do
            yield!
                seq {
                    for y' in max 0 (y - 1) .. min (GRIDSIZE - 1) (y + 1) do
                        if (x', y') <> (x, y) then (x', y')
                }
    }

let runStep grid =

    let getFlashingNeighbours (grid: Grid) x y =
        getNeighbours x y
        |> Seq.filter (fun (nx, ny) -> grid.[nx].[ny] > MAX_ENERGY)
        |> Seq.length

    // Work out the new energy for this cell based
    let handleCellFlashes grid x y energy =
        // Skip over any nodes already flashed - we don't want to increment them again
        if energy = 0 then
            0
        elif energy > MAX_ENERGY then
            // Trigger flash, nothing more to do.
            0
        else
            // Increment energy by the number of adjacent nodes that will flash this turn
            energy + getFlashingNeighbours grid x y

    let rec triggerFlashes grid =
        let newGrid =
            grid
            |> Array.mapi (fun x row ->
                row
                |> Array.mapi (fun y energy -> handleCellFlashes grid x y energy))

        if newGrid = grid then
            newGrid
        else
            triggerFlashes newGrid

    let grid =
        grid
        // Stage 1 - increment all values by 1
        |> Array.map (Array.map (fun energy -> energy + 1))
        // Stage 2 - trigegr flashes
        |> triggerFlashes

    let flashes =
        grid
        |> Array.collect (Array.filter (fun e -> e = 0))
        |> Array.length

    (grid, flashes)

let run (input: string []) =
    let grid: Grid =
        input
        |> Array.map (fun line ->
            line
            |> Seq.map (System.Char.GetNumericValue >> int)
            |> Seq.toArray)

    let checkLength (arr: 'T []) msg =
        if arr.Length <> GRIDSIZE then
            invalidArg $"{arr.Length}" msg

    checkLength grid $"Input must be {GRIDSIZE} lines"
    checkLength grid.[0] $"Rows must be {GRIDSIZE} integers"

    let runPart1 grid numSteps =
        ((grid, 0), seq { 1 .. numSteps })
        ||> Seq.fold (fun (grid, flashes) i ->
            let (newGrid, newFlashes) = runStep grid
            (newGrid, flashes + newFlashes))

    let part1Steps = 100
    let grid, part1 = runPart1 grid part1Steps

    // For part 2, simply keep running through until there are 100 flashes
    let rec runPart2 grid step =
        let (newGrid, newflashes) = runStep grid

        match newflashes with
        | x when x = (GRIDSIZE * GRIDSIZE) -> step
        | _ -> runPart2 newGrid (step + 1)

    let part2 = runPart2 grid (part1Steps + 1)
    int64 part1, int64 part2
