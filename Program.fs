[<EntryPoint>]
let main args =
    if args.Length = 0 then
       printfn $"No arguments provided"
       1
    else
        let day = int args[0]
        match day with
        | 1  -> day1.runDay; 0
        | 2  -> day2.runDay; 0
        | 3  -> day3.runDay; 0
        | 4  -> day4.runDay; 0
        | 5  -> day5.runDay; 0
        |_ -> printfn $"Unimplemented day {day}"; 1
