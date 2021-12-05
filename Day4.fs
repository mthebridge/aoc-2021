module day4

type Cell =
    { value: int
      mutable marked: bool }

    member Cell.maybeMark(value) =
        if Cell.value = value then
            Cell.marked <- true

let BOARDSIZE = 5

type Board(values: int [] []) =


    do
        if values.Length <> (BOARDSIZE) then
            invalidArg "" $"Boards must be {BOARDSIZE} by {BOARDSIZE} cells"

        if
            not
                (
                    values
                    |> Array.forall (fun row -> row.Length = BOARDSIZE)
                )
        then
            invalidArg "" $"Boards must be {BOARDSIZE} by {BOARDSIZE} cells"

    // Initialize the cells.
    let cells =
        values
        |> Array.map (fun row ->
            row
            |> Array.map (fun v -> { Cell.value = v; marked = false }))

    let mutable wonInner = false

    // Has the baord been won?
    member this.won = wonInner

    // Mark a value as called.  Assumes each value only occurs once per board!
    member this.markValue(value) =
        if not this.won then
            cells |> Array.iter (fun row -> row |> Array.iter (fun c -> c.maybeMark value))
            wonInner <- this.checkHasWon()

    // Has the board got a line
    member this.checkHasWon() =
        let is_win line =
            line |> Array.forall (fun c -> c.marked)

        cells |> Array.exists is_win
            || cells |> Array.transpose |> Array.exists is_win


    // what is the sum of all unmarked
    member this.sumUnmarked() =
        cells
        |> Array.sumBy (fun row ->
            row
            |> Array.sumBy (fun c -> if not c.marked then c.value else 0))


let runDay =
    let input =
        System.IO.File.ReadAllLines(@"day4input.txt")

    // First line is a list of input numbers
    let numbers = input.[0].Split ',' |> Array.map int

    let boards =
        input.[1..]
        // Chunk +1 because of the blank lines
        |> Array.chunkBySize (BOARDSIZE + 1)
        // Iterate over each board
        |> Array.map (fun boardRows ->
            let boardValues =
                boardRows
                |> Array.skip 1
                |> Array.map (fun line ->
                    // Map each line to a set of numbers
                    // printfn $"Parsing line {line}"
                    line.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int)
            Board(boardValues))

    let findNextWinningBoard (boardList: Board[], num) =
        let markAndCheckWon (board: Board, num) =
            let wasWon = board.won
            board.markValue num
            board.won && not wasWon
        match boardList |> Array.tryFind (fun board -> markAndCheckWon(board, num)) with
                |Some(board) -> Some((board))
                |None -> None

    let (last, firstWinningBoard) =
        numbers |>
            Array.pick (fun num -> findNextWinningBoard(boards, num) |> Option.map (fun b -> (num, b)))

    let score = firstWinningBoard.sumUnmarked()
    printfn $"Part 1: {score * last} = {score} * {last}"

    // Run again until all boards are won
    let mutable boardsWon = 0

    let (last, lastWinningBoard) =
        let mutable boardsWon = 0
        numbers |>
            Array.pick (fun num ->
                    // Keep iterating until all boards are won
                    match findNextWinningBoard(boards, num) with
                        |Some(board) -> boardsWon <- (boardsWon + 1); printfn $"{boardsWon}/{boards.Length} boards now Won"; if boardsWon = boards.Length then Some((num, board)) else None
                        |None -> None
            )

    let score = lastWinningBoard.sumUnmarked()
    printfn $"Part 2: {score * last} = {score} * {last}"

