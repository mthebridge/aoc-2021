module day4

let expectedTest = (4512L, 1924L)

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

    member this.reset() =
        cells
        |> Array.iter (fun r -> r |> Array.iter (fun c -> c.marked <- false))

        wonInner <- false

    // Has the baord been won?
    member this.won = wonInner

    // Mark a value as called.  Assumes each value only occurs once per board!
    member this.markValue(value) =
        if not this.won then // Can skip checking if the board has already won
            cells
            |> Array.iter (fun row -> row |> Array.iter (fun c -> c.maybeMark value))

            wonInner <- this.checkHasWon ()

    // Has the board got a line
    member this.checkHasWon() =
        let is_win line =
            line |> Array.forall (fun c -> c.marked)

        cells |> Array.exists is_win
        || cells |> Array.transpose |> Array.exists is_win


    // what is the sum of all unmarked
    member this.getScore(last) =
        last * (cells
        |> Array.sumBy (fun row ->
            row
            |> Array.sumBy (fun c -> if not c.marked then c.value else 0)))

let run (input: string[]) =

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
                    line.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map int)

            Board(boardValues))

    let findNextWinningBoard (boardList: Board [], num) =
        let markAndCheckWon (board: Board, num) =
            let wasWon = board.won
            board.markValue num
            board.won && not wasWon

        (None, boardList)
        ||> Array.fold (fun lastWin board ->
            if markAndCheckWon (board, num) then
                Some(board)
            else
                lastWin)

    let (last, firstWinningBoard) =
        numbers
        |> Array.pick (fun num ->
            findNextWinningBoard (boards, num)
            |> Option.map (fun b -> (num, b)))

    let part1 = firstWinningBoard.getScore(last)

    // Run again until all boards are won
    let mutable boardsWon = 0
    do boards |> Array.iter (fun b -> b.reset ())

    let (last, lastWinningBoard) =
        ((last, None), numbers)
        ||> Array.fold (fun (last_num, last_board) num ->
            // Keep iterating until all boards are won
            if boardsWon = boards.Length then
                (last_num, last_board)
            else
                match findNextWinningBoard (boards, num) with
                | Some (board) ->
                    boardsWon <- (boardsWon + 1)
                    // printfn $"{num} has triggered a board win (now {boardsWon})"
                    (num, Some(board))
                | None -> (last_num, last_board))

    let part2 = lastWinningBoard.Value.getScore(last)
    int64 part1, int64 part2
