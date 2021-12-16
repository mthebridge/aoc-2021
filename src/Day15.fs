module day15

let expectedTest = (40L, 315L)

type Grid = int [] []
type Point = int * int

let runInner (grid: Grid) =
    let maxX = grid.[0].Length - 1
    let maxY = grid.Length - 1
    printfn $"Grid max is {maxX} by {maxY}"

    let getNeighbours (grid: Grid) (x: int) (y: int) : seq<Point> =
        seq {
            for (x', y') in
                seq {
                    (x + 1, y)
                    (x, y + 1)
                    (x, y - 1)
                    (x - 1, y)
                } do
                if x' >= 0 && x' <= maxX && y' >= 0 && y' <= maxY then
                    (x', y')
        }

    let mutable pointScores = Map.empty
    let mutable visited = Set.empty

    let rec visitPoint score (x, y) =
        visited <- visited.Add(x, y)
        pointScores <- pointScores.Remove(x, y)
        // For each neighbour, update the possible scores.
        getNeighbours grid x y
        |> Seq.where (fun n -> not (visited.Contains n))
        |> Seq.iter (fun (nx, ny) ->
            let thisScore = score + grid.[ny].[nx]

            let thisPtScore =
                match pointScores |> Map.tryFind (nx, ny) with
                | Some (s) -> min s thisScore
                | None -> thisScore
            // printfn $"Checking ({nx},{ny}) - {thisPtScore} <= {thisScore}"
            pointScores <- (pointScores |> Map.add (nx, ny) thisPtScore))

        let (best, newScore) =
            pointScores |> Map.toSeq |> Seq.minBy snd
        // printfn $"Best so far {best}: {newScore}"
        if best = (maxX, maxY) then
            newScore
        else
            visitPoint newScore best

    pointScores <- (pointScores |> Map.add (0, 0) 0)
    visitPoint 0 (0, 0)

let run (input: string []) =
    let grid: Grid =
        input
        |> Array.map (fun line ->
            line
            |> Seq.map (System.Char.GetNumericValue >> int)
            |> Seq.toArray)

    let part1 = runInner grid

    // For part2 we need to create the new grid.
    let grid2 =
        [| 0 .. (5 * grid.Length) - 1 |]
        |> Array.map (fun y ->
            [| 0 .. (5 * grid.[0].Length) - 1 |]
            |> Array.map (fun x ->
                let origx = x % grid.[0].Length
                let origy = y % grid.Length

                let diff =
                    (x / grid.[0].Length) + (y / grid.Length)
                // printfn $"{x},{y} maps to {origx},{origy} but {diff} higher"
                (grid.[origy].[origx] + diff - 1) % 9 + 1

            ))

    let part2 = runInner grid2

    int64 part1, int64 part2
