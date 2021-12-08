module day8

let expectedTest = (26L, 61229L)

type Digit = Set<char>

type Entry =
    { digits: array<Digit>
      outputs: array<Digit> }

type PossibleValues =
    | Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | TwoThreeFive
    | ZeroSixNine


let run (input: string []) =

    let entries =
        input
        |> Array.map (fun line ->
            let parts = line.Split "|"

            { digits =
                (parts.[0]
                    .Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
                 |> Array.map Set.ofSeq)
              outputs =
                parts.[1]
                    .Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map Set.ofSeq })

    // For part1 we just want the number of outputs with length 2,3, 4 or 7
    let isUniqueDigit (output: Digit) =
        output.Count = 2
        || output.Count = 3
        || output.Count = 4
        || output.Count = 7

    let part1 =
        (0, entries)
        ||> Array.fold (fun count entry ->
            count
            + (entry.outputs |> Array.filter isUniqueDigit)
                .Length)

    // For each entry:
    // -> Work out what letters correspond to 1, 4, 7, 8 (2, 4, 3, 7 segments)
    // -> For the remaining digits, 2, 3 and 5 are 5 segment; 0, 6 and 9 are 6-segment
    let getValue poss =
        match poss with
        | Zero -> 0
        | One -> 1
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5
        | Six -> 6
        | Seven -> 7
        | Eight -> 8
        | Nine -> 9
        | TwoThreeFive
        | ZeroSixNine -> invalidOp "Cannot convert to digit when still unknown"

    let determineDigits (digits: Digit []) =
        let mapping =
            seq {
                for d in digits ->
                    (d,
                     match d.Count with
                     | 2 -> PossibleValues.One
                     | 3 -> PossibleValues.Seven
                     | 4 -> PossibleValues.Four
                     | 5 -> PossibleValues.TwoThreeFive
                     | 6 -> PossibleValues.ZeroSixNine
                     | 7 -> PossibleValues.Eight
                     | _ -> invalidArg $"{d}" "Not a valid digit")
            }
            |> Map.ofSeq
        // Now work out the tricky ones
        //     -> 3 can be distinguished from 2 and 5 by having 7 as a subset
        //     -> 2 and 5 can be distinguished by having 2 or 3 in common with 4
        //     -> 6 can be distinguished from 0 and 9 by not having 7 as a subset
        //     -> 9 can be distinguished from 0 and 6 by having 4 as a subset
        let seven =
            mapping |> Map.findKey (fun _ v -> v = Seven)

        let four =
            mapping |> Map.findKey (fun _ v -> v = Four)

        mapping
        |> Map.map (fun digit value ->
            match value with
            | PossibleValues.TwoThreeFive ->
                if seven.IsSubsetOf digit then
                    3
                elif ((four, digit) ||> Set.intersect).Count = 2 then
                    2
                else
                    5
            | PossibleValues.ZeroSixNine ->
                if four.IsSubsetOf digit then 9
                elif seven.IsSubsetOf digit then 0
                else 6
            | x -> getValue x)

    let part2 =
        (0, entries)
        ||> Array.fold (fun sum entry ->
            let digitMap = determineDigits entry.digits
            // work out the output a digit at a time, build it up base 10
            let thisValue =
               (0, entry.outputs)
               ||> Array.fold (fun v output -> 10 * v + digitMap.Item output)
            sum + thisValue
        )

    int64 part1, int64 part2
