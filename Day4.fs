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

    member this.reset() =
        cells |> Array.iter (fun r -> r |> Array.iter (fun c -> c.marked <- false))
        wonInner <- false

    // Has the baord been won?
    member this.won = wonInner

    // Mark a value as called.  Assumes each value only occurs once per board!
    member this.markValue(value) =
        if not this.won then // Can skip checking if the board has already won
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
    // let input = [|
    //     "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1";
    //     "";
    //     "22 13 17 11  0";
    //      "8  2 23  4 24";
    //     "21  9 14 16  7";
    //      "6 10  3 18  5";
    //      "1 12 20 15 19";
    //     "";
    //      "3 15  0  2 22";
    //      "9 18 13 17  5";
    //     "19  8  7 25 23";
    //     "20 11 10 24  4";
    //     "14 21 16 12  6";
    //     "";
    //     "14 21 17 24  4";
    //     "10 16 15  9 19";
    //     "18  8 23 26 20";
    //     "22 11 13  6  5";
    //     "2  0 12  3  7";
    // |]

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
        (None, boardList) ||> Array.fold (fun lastWin board ->
            if markAndCheckWon(board, num) then
                Some(board)
                else lastWin
            )

    let (last, firstWinningBoard) =
        numbers |>
            Array.pick (fun num -> findNextWinningBoard(boards, num) |> Option.map (fun b -> (num, b)))

    let score = firstWinningBoard.sumUnmarked()
    printfn $"Part 1: {score * last} = {score} * {last}"

    // Run again until all boards are won
    let mutable boardsWon = 0
    do boards |> Array.iter (fun b -> b.reset() )

    let (last, lastWinningBoard) =
        ((last, None), numbers)
        ||> Array.fold (fun (last_num, last_board) num ->
                // Keep iterating until all boards are won
                match findNextWinningBoard(boards, num) with
                    |Some(board) -> boardsWon <- (boardsWon + 1); printfn $"{num} has triggered a board win (now {boardsWon})"; (num, Some(board))
                    |None -> (last_num, last_board)
            )

    let score = lastWinningBoard.Value.sumUnmarked()
    printfn $"Part 2: {score * last} = {score} * {last}"
