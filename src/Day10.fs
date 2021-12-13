module day10

let expectedTest = (26397L, 288957L)

/// Parse a line and retruns a tuple of:
///  - the "stack" of incomplete openers
///  - the first invalid char
let getLineData (line: string) =

    (([], None), line)
    ||> Seq.fold (fun (stack, bad) c ->
        if bad.IsSome then
            (stack, bad)
        else
            match c with
            | '('
            | '<'
            | '{'
            | '[' -> (c :: stack, None)
            | ')'
            | '>'
            | '}'
            | ']' ->
                match stack with
                | last :: tail ->
                    match last with
                    | '(' when c <> ')' -> (stack, Some(c))
                    | '[' when c <> ']' -> (stack, Some(c))
                    | '<' when c <> '>' -> (stack, Some(c))
                    | '{' when c <> '}' -> (stack, Some(c))
                    | _ -> (tail, None)
                | [] -> (stack, Some(c))
            | _ -> invalidArg $"{c}" "Invalid character!")

let run (input: string []) =
    let part1 =
        (0, input)
        ||> Array.fold (fun total line ->
            let (_, badchar) = getLineData line

            total
            + match badchar with
              | Some (')') -> 3
              | Some (']') -> 57
              | Some ('}') -> 1197
              | Some ('>') -> 25137
              | Some (c) -> invalidArg $"{c}" "Impossible bad character"
              | None -> 0)

    let part2Scores =
        input
        |> Array.choose (fun line ->
            let (stack, badChar) = getLineData line
            // Discard the corrupt lines.
            if badChar.IsSome then
                None
            else
                let lineScore =
                    stack
                    |> List.fold
                        (fun score char ->
                            (score * 5L)
                            + match char with
                              | '(' -> 1L
                              | '[' -> 2L
                              | '{' -> 3L
                              | '<' -> 4L
                              | _ -> invalidArg $"{char}" "Impossible bad character")
                        0L

                Some(lineScore))
        |> Array.sort

    let medIdx = part2Scores.Length / 2
    let part2 = part2Scores.[medIdx]

    int64 part1, part2
