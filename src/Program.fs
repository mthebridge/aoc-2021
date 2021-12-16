[<EntryPoint>]
let main args =
    if args.Length = 0 then
       printfn $"No arguments provided"
       1
    else
        let day = int args[0]
        let test = if args.Length > 1 then args.[1] = "test" else false

        let (runfn, (part1exp, part2exp)) =
            match day with
            | 1  -> day1.run, day1.expectedTest
            | 2  -> day2.run, day2.expectedTest
            | 3  -> day3.run, day3.expectedTest
            | 4  -> day4.run, day4.expectedTest
            | 5  -> day5.run, day5.expectedTest
            | 6  -> day6.run, day6.expectedTest
            | 7  -> day7.run, day7.expectedTest
            | 8  -> day8.run, day8.expectedTest
            | 9  -> day9.run, day9.expectedTest
            | 10  -> day10.run, day10.expectedTest
            | 11  -> day11.run, day11.expectedTest
            | 12  -> day12.run, day12.expectedTest
            | 13  -> day13.run, day13.expectedTest
            | 14  -> day14.run, day14.expectedTest
            | 15  -> day15.run, day15.expectedTest
            | 16  -> day16.run, day16.expectedTest
            | 17  -> day17.run, day17.expectedTest
            |_ -> invalidArg $"{day}" "Unimplemented day"

        let input = System.IO.File.ReadAllLines(if test then  $@"inputs/test{day}.txt"  else $@"inputs/day{day}.txt")

        let (part1, part2) = runfn input

        if test then
            if part1exp <> part1 then
                printfn $"Part 1 test failed! Expected {part1exp} Got {part1}"; 1
            elif part2exp <> part2 then
                printfn $"Part 2 test failed! Expected {part2exp} Got {part2}"; 1
            else
                printfn "Tests passed!"; 0
        else
            printfn $"Part1: {part1}\nPart2: {part2}"
            0
