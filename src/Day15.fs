module day15

let expectedTest = (40L, 0L)

type Grid = int [] []
type Point= int * int

type Path = Set<Point>





let run (input: string[]) =
    let grid: Grid =
        input
        |> Array.map (fun line ->
            line
            |> Seq.map (System.Char.GetNumericValue >> int)
            |> Seq.toArray)

    let maxX = grid.[0].Length
    let maxY = grid.Length
    let getNeighbours (grid: Grid) x y : Set<Point> =
        seq {
            for x' in seq { (x - 1); (x + 1) } do
                yield!
                    seq {
                        for y' in seq {(y - 1); (y + 1)} do
                            if x' > 0 && x' < maxX && y' > 0 && y' < maxY then (x', y')
                    }
        }
        |> Set.ofSeq

    let rec getScoresFromPoint score point visited =
        let (x, y) = point
        ((None, score), getNeighbours grid point)
        ||> Array.fold (fun (bestScore, thisScore) n ->
            if visited.Contains(n) then score
            else
                let (nx, ny) = n
                let newScore = score + grid.[ny].[nx]
                if bestScore.isSome && newScore >= bestScore.Value then
                    // Not best, return original
                    (bestScore, score)
                elif nx = maxX && ny = maxY then
                    (newScore, newScore)
                else
                    getScoreFromPoint  n (visited.Add n)
        )

    let visited = Set seq { (0.0) }
    let (part1, _) = getScoresFromPoint grid.[0].[0] (0, 0) visited

    int64 part1, 0L
