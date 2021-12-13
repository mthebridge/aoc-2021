module day13

let expectedTest = (17L, 0L)

let parseHole (line: string) =
    let s = line.Split "," |> Array.map int

    if s.Length <> 2 then
        invalidArg line "Invalid coordinate"

    s.[0], s.[1]

type FoldDir =
    | XFold
    | YFold

type FoldInstr = { dir: FoldDir; pos: int }

let parseFold (line: string) =
    let words = line.Split " "

    if not (line.StartsWith "fold along " && words.Length = 3) then
        invalidArg line "Invalid fold"

    let s = words.[2].Split "="

    let dir =
        match s.[0] with
        | "x" -> XFold
        | "y" -> YFold
        | _ -> invalidArg s.[0] "Invalid Fold axis"

    let pos = int s.[1]
    { dir = dir; pos = pos }

let runFold (holes: Set<int * int>) instr =
    let partitioner =
        match instr.dir with
        | XFold -> (fun h -> fst h < instr.pos)
        | YFold -> fun h -> snd h < instr.pos

    let (left, right) = holes |> Set.partition partitioner

    right
    |> Set.map (fun (x, y) ->
        match instr.dir with
        // Flip the relevant cooridnate relative to the fold line
        | XFold -> ((2 * instr.pos - x), y)
        | YFold -> (x, (2 * instr.pos - y)))
    |> Set.union left


let run (input: string []) =
    let splitIdx =
        input |> Array.findIndex (fun s -> s.Length = 0)

    let holes =
        input.[..splitIdx - 1]
        |> Array.map parseHole
        |> Set.ofArray

    let folds =
        input.[(splitIdx + 1)..] |> Array.map parseFold

    let part1 = runFold holes folds.[0] |> Set.count

    let image = (holes, folds) ||> Array.fold runFold

    // We can't return an image, so just print it and return 0
    let maxX = image |> Set.map fst |> Set.maxElement
    let maxY = image |> Set.map snd |> Set.maxElement

    for y in 0 .. maxY do
        for x in 0 .. maxX do
            if image.Contains(x, y) then
                printf "X"
            else
                printf " "

        printfn ""

    int64 part1, 0L
