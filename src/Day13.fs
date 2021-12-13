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

    // Split the holes into the two "halves"
    let (left, right) = holes |> Set.partition (fun h ->
        match instr.dir with
        | XFold -> fst h < instr.pos
        | YFold -> snd h < instr.pos
    )

    // Reflecting n across the fold means n -> fold - (n - fold)
    // which simplifies to the below.
    let flipcoord n = (2 * instr.pos) - n

    right
    |> Set.map (fun (x, y) ->
        match instr.dir with
        // Flip the relevant coordinate relative to the fold line
        | XFold -> (flipcoord x, y)
        | YFold -> (x, flipcoord y))
    |> Set.union left

let printImage image =
    let maxX = image |> Set.map fst |> Set.maxElement
    let maxY = image |> Set.map snd |> Set.maxElement

    for y in 0 .. maxY do
        for x in 0 .. maxX do
            if image.Contains(x, y) then
                printf "X"
            else
                printf " "

        printfn ""

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

    // We can't return an image, so just print it and return 0
    do
        (holes, folds)
        ||> Array.fold runFold
        |> printImage

    int64 part1, 0L
