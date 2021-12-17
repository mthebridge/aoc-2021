module day17

let expectedTest = (45L, 112L)

type Target =
    { xmin: int
      xmax: int
      ymin: int
      ymax: int }

type Probe =
    { posx: int
      posy: int
      velx: int
      vely: int
      maxyp: int
      hit: bool }

let newProbe vx vy =
    { posx = 0
      posy = 0
      velx = vx
      vely = vy
      maxyp = 0
      hit = false }

let passed probe target =
    // The probe can be ignored once:
    // - posx > targetxmax OR posy <targetymin AND velocity is downward.
    probe.posx > target.xmax
    || (probe.posy < target.ymin && probe.vely < 0)


let hitsArea probe target =
    probe.posx >= target.xmin
    && probe.posx <= target.xmax
    && probe.posy >= target.ymin
    && probe.posy <= target.ymax

let runStep probe target =
    let movetozero v =
        if v < 0 then v + 1
        elif v > 0 then v - 1
        else 0

    let newypos = probe.posy + probe.vely
    { posx = probe.posx + probe.velx
      posy = newypos
      velx = movetozero probe.velx
      vely = probe.vely - 1
      maxyp = max probe.maxyp newypos
      hit = probe.hit || hitsArea probe target}

// Calculate the path from thie starting velocity.
let checkTrajectory vx vy target =
    // printfn $"Trying {vx},{vy}..."
    let mutable probe = newProbe vx vy

    while not (passed probe target) do
        probe <- runStep probe target

    // printfn $"  Hit: {probe.hit}"
    if probe.hit then Some probe else None

let parse (input: string) =
    let words = input.Split " "

    let parsecoord (word: string) =
        let trimmed = word.TrimEnd [| ',' |]

        ((trimmed.Split "=", 1) ||> Array.get).Split ".."
        |> Array.map int

    let xrange = parsecoord words.[2]
    let yrange = parsecoord words.[3]

    { xmin = xrange.[0]
      xmax = xrange.[1]
      ymin = yrange.[0]
      ymax = yrange.[1] }

let run (input: string []) =

    let target = parse input.[0]
    // We need to find vx, vy st that checkTrajectory rteurns the maximum.
    // We can set some bounds:
    // initial vx must be > 0 and < target xmax
    // initial vy is trickier.
    //   A minimum bound is target ymin (because we'll overshoot below otherwise).
    //   Upper bound can be xmax - since we drop by at least 1 a turn, if we had *higher
    let candidates =
        seq {
            for vx in seq { 1 .. target.xmax } do
                yield!
                    seq {
                        for vy in seq { target.ymin .. target.xmax - target.ymin } do
                            (vx, vy)
                    }
        }

    let valid =
        candidates
        |> Seq.choose (fun (vx, vy) -> checkTrajectory vx vy target)
    let part1 = valid |> Seq.map (fun p -> p.maxyp) |> Seq.max
    let part2 = valid |> Seq.length

    int64 part1, int64 part2
