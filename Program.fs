﻿module day1 =
    let input =
        System.IO.File.ReadAllLines(@"day1input.txt")
        |> Array.map int

    let is_incr (x, y) = x < y

    let count_incr_steps list =
        list
        |> Array.pairwise
        |> Array.filter is_incr
        |> Array.length

    let incrs =
        input |> count_incr_steps


    printfn $"Part 1: {incrs}"

    let threewaysum (list: array<int>) =
        let limit = list.Length - 2

        seq { 0 .. list.Length }
        |> Seq.choose (fun (i) ->
            if i >= limit then
                Option.None
            else
                Option.Some(list[i] + list[i + 1] + list[i + 2]))
        |> Seq.toArray

    let newincrs =
        input
        |> threewaysum
        |> count_incr_steps

    printfn $"Part 2: {newincrs}"
