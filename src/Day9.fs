module day9

let expectedTest = (15L, 0L)

type HeightMap = array<array<int>>


let run (input: string []) =
    let heights: HeightMap =
        input
        |> Array.map (fun line -> line |> Seq.map (System.Char.GetNumericValue >> int)|> Seq.toArray)

    let numRows = heights.Length
    let numCols = heights.[0].Length


    let getNeighbours (map: HeightMap) x y =
        let mutable neighbours = Set.empty
        // x =  column index, y = row index => array is y first
        if x > 0 then
            neighbours <- neighbours.Add map.[y].[x - 1]

        if x < numCols - 1 then
             neighbours <- neighbours.Add map.[y].[x + 1]

        if y > 0 then
            neighbours <- neighbours.Add map.[y - 1].[x]

        if y < numRows - 1 then
            neighbours <- neighbours.Add map.[y + 1].[x]

        neighbours

    let part1 =
        (0, { 0 .. numRows - 1 })
        ||> Seq.fold (fun sum row ->
            (sum, seq { 0 .. numCols - 1 })
            ||> Seq.fold (fun sum col ->
                let current = heights.[row].[col]
                // do printfn $"({col}, {row} = {current})"
                let neighbours = getNeighbours heights col row

                sum
                 + if neighbours |> Set.forall (fun neigh -> current < neigh) then
                        printfn $"Have low point at ({col}, {row})"
                        current + 1
                    else
                        0))

    // Work out how big the "basin" is for each low point.
    int64 part1, 0L
