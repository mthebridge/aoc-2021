module day16

let expectedTest = (20L, 1L)

let hexToBin c =
    match c with
    | '0' ->
        seq {
            0
            0
            0
            0
        }
    | '1' ->
        seq {
            0
            0
            0
            1
        }
    | '2' ->
        seq {
            0
            0
            1
            0
        }
    | '3' ->
        seq {
            0
            0
            1
            1
        }
    | '4' ->
        seq {
            0
            1
            0
            0
        }
    | '5' ->
        seq {
            0
            1
            0
            1
        }
    | '6' ->
        seq {
            0
            1
            1
            0
        }
    | '7' ->
        seq {
            0
            1
            1
            1
        }
    | '8' ->
        seq {
            1
            0
            0
            0
        }
    | '9' ->
        seq {
            1
            0
            0
            1
        }
    | 'A' ->
        seq {
            1
            0
            1
            0
        }
    | 'B' ->
        seq {
            1
            0
            1
            1
        }
    | 'C' ->
        seq {
            1
            1
            0
            0
        }
    | 'D' ->
        seq {
            1
            1
            0
            1
        }
    | 'E' ->
        seq {
            1
            1
            1
            0
        }
    | 'F' ->
        seq {
            1
            1
            1
            1
        }
    | _ -> invalidArg $"{c}" "Not a hex digit"

let binToInt bin =
    (0, bin)
    ||> Seq.fold (fun total x -> 2 * total + x)

let binToU64 bin =
    (0UL, bin)
    ||> Seq.fold (fun total x -> 2UL * total + uint64 x)

type PacketHeader = { ptype: int; version: int }

type Packet =
    | Literal of uint64
    | Wrapper of list<PacketHeader * Packet>

let rec VersionSum pkt =
    match pkt with
    | Literal (_) -> 0
    | Wrapper (inner) ->
        (0, inner)
        ||> List.fold (fun sum (hdr, pkt) -> sum + hdr.version + VersionSum pkt)

let rec evaluate ptype pkt =
    match pkt with
    | Literal (x) -> x
    | Wrapper (inner) ->
        let inners =
            inner
            |> List.map (fun (h, p) -> evaluate h.ptype p)

        match ptype with
        | 0 -> inners |> List.sum
        | 1 -> inners |> List.fold (fun acc v -> acc * v) 1UL
        | 2 -> inners |> List.min
        | 3 -> inners |> List.max
        | 5 ->
            if inners.[0] > inners.[1] then
                1UL
            else
                0UL
        | 6 ->
            if inners.[0] < inners.[1] then
                1UL
            else
                0UL
        | 7 ->
            if inners.[0] = inners.[1] then
                1UL
            else
                0UL
        | _ -> invalidArg $"{ptype}" "unknown packet operation"

type LengthType =
    | PktCount of int
    | PktLength of int

let parseValue body =
    let mutable seenFinal = false

    let (num, consumed) =
        body
        |> List.chunkBySize 5
        |> List.mapFold
            (fun parsed chunk ->
                if not seenFinal then
                    if chunk.[0] = 1 then
                        chunk.[1..], parsed + 5
                    else
                        seenFinal <- true
                        chunk.[1..], parsed + 5
                else
                    List.empty, parsed)
            0
    // printf "Binary rep: "
    // for b in num |> List.collect id do
    //     printf $"{b}"
    // printfn $" - consumed {consumed}"

    let value =
        num |> List.collect id |> binToU64 |> Literal

    printfn $"Got literal {value}"
    value, body |> List.skip consumed

let rec parseWrapper lType (data: list<int>) =
    match lType with
    | PktCount (c) ->
        // There are N packets in data.  Keep parsing until we have N.
        // printfn $"{c} inner packets"

        let pkts, rest =
            (data, seq { 1 .. c })
            ||> Seq.mapFold (fun d _ -> parsePacket d)

        (pkts |> Seq.toList |> Wrapper, rest |> Seq.toList)
    | PktLength (l) ->
        // There are packets totalling L bytes
        // printfn $"{l} inner bytes"
        let toParse = data |> List.take l

        let pkts =
            toParse
            |> List.unfold (fun d ->
                if d.IsEmpty then
                    None
                else
                    Some(parsePacket d))

        (pkts |> Wrapper, data |> List.skip l)

and parsePacket binary =
    // Header
    let version, rest = binary |> List.splitAt 3
    let ptype, body = rest |> List.splitAt 3

    let header =
        { ptype = binToInt ptype
          version = binToInt version }

    printfn $"Next packet type {header.ptype} v{header.version}"

    let pkt, rest =
        match header.ptype with
        | 4 -> parseValue body
        | _ ->
            let pLength, pBody =
                match body.Head with
                | 0 ->
                    let count, rest = body.Tail |> List.splitAt 15
                    (binToInt count |> PktLength, rest)
                | 1 ->
                    let count, rest = body.Tail |> List.splitAt 11
                    (binToInt count |> PktCount, rest)
                | x -> invalidArg $"{x}" "Not a binary digit"

            parseWrapper pLength pBody

    (header, pkt), rest

let run (input: string []) =
    let (hdr, packet), _ =
        input.[0]
        |> Seq.collect hexToBin
        |> Seq.toList
        |> parsePacket

    let part1 = hdr.version + VersionSum packet
    let part2 = evaluate hdr.ptype packet
    int64 part1, int64 part2
