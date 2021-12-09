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

    let lowPoints =
        { 0 .. numRows - 1 }
        |> Seq.collect (fun row ->
            { 0 .. numCols - 1 }
            |> Seq.choose (fun col ->
                let current = heights.[row].[col]
                let neighbours = getNeighbours heights col row
                if neighbours |> Set.forall (fun neigh -> current < neigh) then
                    Some((row, col))
                else
                    None
            ))

    let part1 =
        (0, lowPoints)
        ||> Seq.fold (fun sum (row, col) ->
            //printfn $"Adding low point at ({col},{row})"
            sum + heights.[row].[col] + 1)

    // Work out how big the "basin" is for each low point.
    int64 part1, 0L
