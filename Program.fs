
module day1 =

    let input = System.IO.File.ReadAllLines(@"day1input.txt")
    let is_incr (x, y) =  x <  y
    let incrs  =
      input
        |> Array.map int
        |> Array.pairwise
        |> Array.filter is_incr
        |> Array.length

    printfn $"Part 1: {incrs}"
