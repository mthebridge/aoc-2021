module day11

let expectedTest = (1656L, 195L)

let GRIDSIZE = 10
type Grid = int [] []

let printGrid grid =
    grid
    |> Array.iter (fun row ->
        row |> Array.iter (fun e -> printf $"{e}")
        printfn ""
    )

let getNeighbours x y =
    let mutable neighs = Set.empty

    if x > 0 then
        neighs <- neighs.Add(x - 1, y)

        if y > 0 then
            neighs <- neighs.Add(x - 1, y - 1)
            neighs <- neighs.Add(x, y - 1)

        if y < GRIDSIZE - 1 then
            neighs <- neighs.Add(x - 1, y + 1)
            neighs <- neighs.Add(x, y + 1)

    if x < GRIDSIZE - 1 then
        neighs <- neighs.Add(x + 1, y)
        if y > 0 then
            neighs <- neighs.Add(x + 1, y - 1)
            neighs <- neighs.Add(x, y - 1)
        if y < GRIDSIZE - 1 then
            neighs <- neighs.Add(x + 1, y + 1)
            neighs <- neighs.Add(x, y + 1)
    let printNeigh (x, y) = $"{x}, {y}"
    neighs


let runStep grid =
    let rec triggerFlashes grid =
        let newGrid =
            grid
            // Increment any fields whose neightbours are > 9
            |> Array.mapi (fun x row ->
                row
                |> Array.mapi (fun y energy ->
                    // Skip over any nodes already flashed - we don't want to increment them again
                    if energy = 0 then
                        0
                    else
                        // For each neighbour that is greater than 9, then increment again
                        let flashedNeighbours =
                            getNeighbours x y
                            |> Set.filter (fun (nx, ny) -> grid.[nx].[ny] > 9 )
                            |> Set.count
                        // if flashedNeighbours > 0 then printfn $"{flashedNeighbours} flashed this loop for ({x},{y})"
                        if energy > 9 then 0
                        else energy + flashedNeighbours
                ))

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

    let checkArg (arr: 'T []) msg =
        if arr.Length <> GRIDSIZE then
            invalidArg $"{arr.Length}" msg

    checkArg grid "Input must be 10 lines"
    checkArg grid.[0] "Rows must be 10 integers"

    let runPart1 grid numSteps =
        ((grid, 0), seq {1 .. numSteps }) ||>
        Seq.fold (fun (grid, flashes) i ->
            let (newGrid, newFlashes) = runStep grid
            (newGrid, flashes + newFlashes)
        )

    let grid, part1 = runPart1 grid 100

    // For part 2, simply keep running through until there are 100 flashes
    let rec runPart2 grid step =
        let (newGrid, newflashes) = runStep grid
        match newflashes with
        |100 -> step
        |_ -> runPart2 newGrid (step + 1)

    let part2 = runPart2 grid 101
    int64 part1, int64 part2

