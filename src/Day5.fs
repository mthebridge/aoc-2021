module day5

type Point = { x: int; y: int}

let parsePoint (point: string) =
    let split = point.Split ","
    { x = int split[0]; y = int split[1]}

type Line = { start: Point; finish: Point }

let parseLine (line: string) =
    let split = line.Split " -> "
    { start = split[0] |> parsePoint;
      finish = split[1] |> parsePoint }

let getPointsInLine line =
    if line.start.x = line.finish.x then
        if line.start.y > line.finish.y then seq { line.finish.y .. line.start.y } else seq { line.start.y .. line.finish.y }
        |> Seq.map (fun y -> { x = line.start.x; y = y })
    elif line.start.y = line.finish.y then
        if line.start.x > line.finish.x then seq { line.finish.x .. line.start.x } else seq { line.start.x .. line.finish.x }
        |> Seq.map (fun x -> { x = x; y = line.start.y })
    else
        // Diagonal.
        let xincr = if line.start.x > line.finish.x then -1 else 1
        let yincr = if line.start.y > line.finish.y then -1 else 1
        (seq { line.start.x .. xincr .. line.finish.x },seq { line.start.y .. yincr .. line.finish.y }) ||> Seq.zip |> Seq.map (fun (x, y) -> { x = x; y = y} )

let runDay input =

    let allLines = input |> Array.map parseLine

    let hvLines = allLines |> Array.filter (fun l -> (l.start.x = l.finish.x) || (l.start.y = l.finish.y))
    do printfn $"Filtered lines length: {hvLines.Length}"

    let getOverlapsForLines lines =
        let points = lines |> Array.map ( getPointsInLine  >> Seq.toArray ) |> Array.concat

        // for point in points do
        //     printfn $"Point: {point.x},{point.y}"
        // We have a list of points.
        let pointCounts = points |> Array.countBy (fun p -> p)
        do printfn $"Count length: {points.Length}"
        (pointCounts |> Array.filter (fun (_, count) -> count >= 2)).Length
    let hvOverlaps = getOverlapsForLines hvLines
    printfn $"Part 1: {hvOverlaps} overlapping"

    let allOverlaps = getOverlapsForLines allLines
    printfn $"Part 2: {allOverlaps} overlapping"

let testDay() =
    runDay [|
        "0,9 -> 5,9";
        "8,0 -> 0,8";
        "9,4 -> 3,4";
        "2,2 -> 2,1";
        "7,0 -> 7,4";
        "6,4 -> 2,0";
        "0,9 -> 2,9";
        "3,4 -> 1,4";
        "0,0 -> 8,8";
        "5,5 -> 8,2";
    |]
