module day3

let runDay =
    let input =
        System.IO.File.ReadAllLines(@"inputs/day3input.txt")
    // Each line is a binary number, for the first part we might as well keep them as a list
    let parse (line: string) =
        line.ToCharArray()
        |> Array.map (fun c ->
            match c with
            | '0' -> 0
            | '1' -> 1
            | _ -> invalidArg $"{c}" "Invalid binary digit")

    let getMostLeastCommonBit bits =
        let bitCounts = bits |> Array.countBy (fun x -> x = 1)
        // CountBy returns a list of tuples
        let ones =
            match bitCounts
                  |> Array.tryFind (fun (k, _) -> k = true)
                with
            | Some ((_, x)) -> x
            | None -> 0

        let zeros = bits.Length - ones

        if ones >= zeros then (1, 0) else (0, 1)

    let parsed = input |> Array.map parse

    let commonBits =
        parsed
        |> Array.transpose
        |> Array.map getMostLeastCommonBit

    let doubleAndShift x y = x * 2 + y

    let (gamma, epsilon) =
        commonBits
        |> Array.reduce (fun (accGamma, accEpsilon) (nextGamma, nextEpsilon) ->
            (doubleAndShift accGamma nextGamma, doubleAndShift accEpsilon nextEpsilon))

    let ans = gamma * epsilon

    printfn $"Part 1 answer: {ans} = {gamma} * {epsilon}"

    let rec filterList list (idx: int) isHigh =
        let bits =
            list
            |> Array.transpose
            |> Array.map getMostLeastCommonBit

        let bit =
            bits.[idx] |> if isHigh then fst else snd

        let newList =
            list
            |> Array.filter (fun (entry: int []) -> entry.[idx] = bit)

        if newList.Length = 1 then
            newList.[0]
        else
            filterList newList (idx + 1) isHigh

    let oxygen =
        filterList parsed 0 true
        |> Array.reduce doubleAndShift

    let co2 =
        filterList parsed 0 false
        |> Array.reduce doubleAndShift

    let ans = oxygen * co2
    printfn $"Part 2 answer: {ans} = {oxygen} * {co2}"
