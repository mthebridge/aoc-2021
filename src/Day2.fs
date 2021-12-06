module day2

type Instruction =
   | Forward of int
   | Up of int
   | Down of int

let runDay input =
    let parse (line: string) =
        let parts = line.Split ' '
        let dir = parts[0]
        let value = int parts[1]

        match dir with
            |"forward" -> Forward (value)
            |"up" ->  Up(value)
            |"down" ->  Down(value)
            |_ -> invalidArg dir "Invalid instruction"

    let (hdist, depth) =
        ((0, 0), input |> Array.map parse) ||> Array.fold (fun (hdist, depth) instr ->
            match instr with
                | Forward(value) ->  hdist + value, depth
                | Up(value) ->  hdist, depth - value
                | Down(value) -> hdist, depth + value
        )

    let answer = depth * hdist
    printfn $"Part 1 answer: {answer}"

    let ((hdist, depth, _)) =
        ((0, 0, 0), input |> Array.map parse) ||> Array.fold (fun (hdist, depth, aim) instr ->
            match instr with
                | Forward(value) ->  hdist + value, depth + (value * aim), aim
                | Up(value) -> hdist, depth, aim - value
                | Down(value) ->  hdist, depth, aim + value
        )
    let answer = depth * hdist
    printfn $"Part 2 answer: {answer}"
