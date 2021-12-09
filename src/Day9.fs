module day9

let expectedTest = (15L, 1134L)

type HeightMap = array<array<int>>

type Direction =
    | North
    | East
    | South
    | West

let neighbour x y dir =
    match dir with
    | North -> x, (y - 1)
    | East -> (x + 1), y
    | South -> x, (y + 1)
    | West -> (x - 1), y

let run (input: string []) =
    let heights: HeightMap =
        input
        |> Array.map (fun line ->
            line
            |> Seq.map (System.Char.GetNumericValue >> int)
            |> Seq.toArray)

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

                if neighbours
                   |> Set.forall (fun neigh -> current < neigh) then
                    Some((row, col))
                else
                    None))

    let part1 =
        (0, lowPoints)
        ||> Seq.fold (fun sum (row, col) ->
            //printfn $"Adding low point at ({col},{row})"
            sum + heights.[row].[col] + 1)

    // Work out how big the "basin" is for each low point.
    let getBasinSize (map: HeightMap) (sinkrow, sinkcol) =
        // Start at the sink point and look for paths to every point within range
        let willDrain this current = this >= current && this < 9
        let mutable visited = Set.empty
        // Visit a node.  If it is "uphill" from the previous, then mark it visited and
        // visit all neighbours.
        let rec visitPoint x y lastHeight : int =
            if
                x < 0
                || y < 0
                || x > numCols - 1
                || y > numRows - 1
                || visited.Contains(x, y)
            then
                0
            else
                let thisHeight = map.[y].[x]

                if not (willDrain thisHeight lastHeight) then
                    0
                else
                    visited <- visited.Add((x, y))
                    // Count this node, and then recurse over all 4 directions
                    (1, [ North; East; South; West ])
                    ||> List.fold (fun count dir ->
                        let nextx, nexty = neighbour x y dir
                        count + visitPoint nextx nexty thisHeight)

        visitPoint sinkcol sinkrow 0

    let part2 =
        lowPoints
        |> Seq.map (fun pt -> getBasinSize heights pt)
        |> Seq.sort
        |> Seq.rev
        |> Seq.take 3
        |> Seq.fold (fun prod size -> prod * size) 1

    int64 part1, int64 part2
