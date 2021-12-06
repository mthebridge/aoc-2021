
[<EntryPoint>]
let main args =
    if args.Length = 0 then
       printfn $"No arguments provided"
       1
    else
        let day = int args[0]
        let test = if args.Length > 1 then args.[1] = "test" else false
        let input = System.IO.File.ReadAllLines($@"inputs/day{day}input.txt")

        do match day with
            | 1  -> day1.runDay input
            | 2  -> day2.runDay input
            | 3  -> day3.runDay input
            | 4  -> if test then day4.testDay() else day4.runDay input
            | 5  -> if test then day5.testDay() else day5.runDay input
            | 6  -> day6.runDay input
            |_ -> invalidArg  $"{day}" "Unimplemented day"

        0
