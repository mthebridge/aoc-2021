module day12

let expectedTest = (19L, 103L)

type Cave =
    | Small of string
    | Large of string
    | Start
    | End

let parseCave entry =
    match entry with
    | s when s = "start" -> Start
    | s when s = "end" -> End
    | s when s |> Seq.forall System.Char.IsUpper -> Large s
    | s when s |> Seq.forall System.Char.IsLower -> Small s
    | _ -> invalidArg entry "Cave does not parse"

type caveSystem = Map<Cave, Set<Cave>>

let addConnection key value (system: caveSystem) =
    let newval =
        match system.TryFind key with
        | Some (connected) -> connected.Add value
        | None -> Set.singleton value

    system.Add(key, newval)

let newCaveSystem input : caveSystem =
    (Map.empty, input)
    ||> Array.fold (fun system (line: string) ->
        let connections = line.Split "-" |> Array.map parseCave

        if connections.Length <> 2 then
            invalidOp "Bad input line"

        let (first, second) = connections.[0], connections.[1]

        system
        |> addConnection first second
        |> addConnection second first)

let rec countPaths (system: caveSystem) nextLinks (visited: Set<Cave>) =
    (0, nextLinks)
    ||> Set.fold (fun count next ->
        match next with
        // New small cave.  Mark visited and recurse
        | Small (_) when not (visited.Contains next) ->
            count
            + countPaths system (system.TryFind next).Value (visited.Add next)
        // New large cave.  Recurse
        | Large (_) ->
            count
            + countPaths system (system.TryFind next).Value visited
        // Visited cave.  Can't go this way, stop here
        | Start
        | Small (_) -> count
        // End of path.  Stop here and add 1
        | End -> count + 1)

let rec countPaths2 (system: caveSystem) nextLinks (visited: Set<Cave>) doubleVisited =
    (0, nextLinks)
    ||> Set.fold (fun count next ->
        match next with
        // New large cave.  Recurse
        | Large (_) ->
            count
            + countPaths2 system (system.TryFind next).Value visited doubleVisited
        // New small cave.  Mark visited and recurse
        | Small (_) when not (visited.Contains next) ->
            count
            + countPaths2 system (system.TryFind next).Value (visited.Add next) doubleVisited
        | Small (_) when not doubleVisited ->
            // We haven't done a double visit yet, try using this cave twice.
            count
            + countPaths2 system (system.TryFind next).Value (visited.Add next) true

        // Visited cave.  Can't go this way, stop here
        | Start
        | Small (_) -> count
        // End of path.  Stop here and add 1
        | End -> count + 1)

let run (input: string []) =
    let caves = newCaveSystem input
    let start = (caves.TryFind Start).Value
    let part1 = countPaths caves start Set.empty
    let part2 = countPaths2 caves start Set.empty false
    int64 part1, int64 part2
