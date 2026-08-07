namespace Grampus

open System
open System.Text.Json
open System.Text.Json.Serialization
open System.IO

    type Repertoire = {
        Name : string
        Side : int // WHITE or BLACK constants from your Types module
        Lines : Mv list list
        Comments : Map<Mv list, string>
    }

    type RepertoireDto = {
        Name : string
        Side : int
        Lines : Mv list list
        CommentsList : (Mv list * string) list
    }

    module Repertoire =
        let private options = JsonSerializerOptions()
        options.WriteIndented <- true
        options.Converters.Add(JsonStringEnumConverter())
        let private getFileName fol side = 
            let nm = if side = WHITE then "repertoire_white.json" else "repertoire_black.json"
            Path.Combine(fol, nm)
        let private getBackupPath fol side =
            let backupDir = Path.Combine(fol, "backups")
            if not (Directory.Exists(backupDir)) then Directory.CreateDirectory(backupDir) |> ignore
            let timestamp = DateTime.Now.ToString("yyyyMMdd_HHmmss")
            let sideStr = if side = WHITE then "white" else "black"
            Path.Combine(backupDir, sprintf "repertoire_%s_%s.json" sideStr timestamp)
        let save fol (repertoire: Repertoire) =
            let path = getFileName fol repertoire.Side
            if File.Exists(path) then
                let backupPath = getBackupPath fol repertoire.Side
                File.Copy(path, backupPath, true)
            let dto = {
                Name = repertoire.Name
                Side = repertoire.Side
                Lines = repertoire.Lines
                CommentsList = repertoire.Comments |> Map.toList
            }
            let json = JsonSerializer.Serialize(dto, options)
            File.WriteAllText(path, json)
        let loadFromFile path side =
            if File.Exists(path) then
                try 
                    let dto = JsonSerializer.Deserialize<RepertoireDto>(File.ReadAllText(path), options)
                    {
                        Name = dto.Name
                        Side = dto.Side
                        Lines = dto.Lines
                        Comments = Map.ofList dto.CommentsList
                    }
                with _ -> { Name = "New Repertoire"; Side = side; Lines = []; Comments = Map.empty }
            else 
                { Name = "New Repertoire"; Side = side; Lines = []; Comments = Map.empty }
        let load fol (side: int) : Repertoire =
            let path = getFileName fol side
            loadFromFile path side
        let getVersions fol side =
            let backupDir = Path.Combine(fol, "backups")
            if not (Directory.Exists(backupDir)) then []
            else
                let sideStr = if side = WHITE then "white" else "black"
                let pattern = sprintf "repertoire_%s_*.json" sideStr
                Directory.GetFiles(backupDir, pattern)
                |> Array.toList
                |> List.sortByDescending id // Filenames are timestamped, so ID sort is chronological       
        let getRequiredOrientation (repertoire: Repertoire) =
            repertoire.Side
        let rec IsPrefix (a: 'a list) (b: 'a list) =
            match a, b with
            | [], _ -> true
            | _, [] -> false
            | x :: xs, y :: ys -> x = y && IsPrefix xs ys
        let update (repertoire: Repertoire) (history: Mv list) (newMv: Mv) =
            let newPath = history @ [newMv]

            // Rule 1: If the path already exists (or is a prefix of a longer line), return unchanged
            if repertoire.Lines |> List.exists (IsPrefix newPath) then
                repertoire
            else
                let currentTurn = if history.Length % 2 = 0 then WHITE else BLACK

                if currentTurn <> repertoire.Side then
                    // Rule 2: Opponent Side
                    // Check if there is a line that is EXACTLY the current history
                    let exactMatchExists = repertoire.Lines |> List.exists (fun line -> line = history)

                    if exactMatchExists then
                        // Extend that specific line
                        let nextLines = 
                            repertoire.Lines 
                            |> List.map (fun line -> if line = history then newPath else line)
                        { repertoire with Lines = nextLines }
                    else
                        // Add a new variation line
                        { repertoire with Lines = newPath :: repertoire.Lines }

                else
                    // Rule 3: Our Side (Replacement Rule)
                    // 1. Identify which lines to remove (any line starting with the current history)
                    let linesToRemove = 
                        repertoire.Lines |> List.filter (IsPrefix history)

                    // 2. Filter them out
                    let filteredLines = 
                        repertoire.Lines |> List.filter (fun line -> not (IsPrefix history line))
            
                    // 3. Clean up comments associated with the lines being deleted
                    let cleanedComments = 
                        repertoire.Comments 
                        |> Map.filter (fun path _ -> 
                            not (linesToRemove |> List.exists (fun removed -> IsPrefix path removed))
                        )

                    { repertoire with 
                        Lines = newPath :: filteredLines
                        Comments = cleanedComments }        
        let setComment (repertoire: Repertoire) (mvl: Mv list) (comment: string) =
            let pathExists = repertoire.Lines |> List.exists (fun line -> IsPrefix mvl line)
            if not pathExists then repertoire
            else
                let newComments = Map.add mvl comment repertoire.Comments
                { repertoire with Comments = newComments }

    type PositionStats = {
        Attempts : int
        Successes : int
        FailedLastTime : bool
    }

    type TrainingStatsDto = {
        StatsList : (Mv list * PositionStats) list
    }

    module TrainingStats =
        let private options = JsonSerializerOptions()
        do
            options.WriteIndented <- true
            options.Converters.Add(JsonStringEnumConverter())

        let private getFileName fol side = 
            let nm = if side = WHITE then "training_stats_white.json" else "training_stats_black.json"
            Path.Combine(fol, nm)

        let load fol side : Map<Mv list, PositionStats> =
            let path = getFileName fol side
            if File.Exists(path) then
                try
                    let dto = JsonSerializer.Deserialize<TrainingStatsDto>(File.ReadAllText(path), options)
                    Map.ofList dto.StatsList
                with _ -> Map.empty
            else
                Map.empty

        let save fol side (stats: Map<Mv list, PositionStats>) =
            let path = getFileName fol side
            let dto = { StatsList = Map.toList stats }
            let json = JsonSerializer.Serialize(dto, options)
            File.WriteAllText(path, json)

    module Training =
        let getCandidatePositions (repertoire: Repertoire) (startPath: Mv list) : Mv list list =
            // Find all prefixes of all lines in repertoire.Lines that start with startPath
            let allPrefixes =
                repertoire.Lines
                |> List.collect (fun line ->
                    [ for i in startPath.Length .. line.Length do
                          yield List.truncate i line ]
                )
                |> List.distinct
            
            // Filter those where it's our side's turn to move, and there is at least one next move
            allPrefixes
            |> List.filter (fun path ->
                let isOurTurn = 
                    if repertoire.Side = WHITE then
                        path.Length % 2 = 0
                    else
                        path.Length % 2 = 1
                
                if not isOurTurn then false
                else
                    // Check if there is at least one move after this path in the repertoire
                    repertoire.Lines
                    |> List.exists (fun line -> 
                        line.Length > path.Length && Repertoire.IsPrefix path line
                    )
            )

        let selectTrainingPositions (repertoire: Repertoire) (startPath: Mv list) (stats: Map<Mv list, PositionStats>) : Mv list list =
            let candidates = getCandidatePositions repertoire startPath
            
            let getPriorityCategory path =
                match Map.tryFind path stats with
                | None -> 0 // Category 0: Untested
                | Some s ->
                    if s.FailedLastTime then 1 // Category 1: Failed last time
                    elif s.Attempts > 0 && s.Successes < s.Attempts then 2 // Category 2: Low success rate
                    else 3 // Category 3: Mastered
                    
            let getSuccessRate path =
                match Map.tryFind path stats with
                | None -> 0.0
                | Some s -> 
                    if s.Attempts = 0 then 0.0
                    else float s.Successes / float s.Attempts

            let rng = Random()
            let sortedCandidates =
                candidates
                |> List.sortBy (fun path ->
                    let cat = getPriorityCategory path
                    let rate = getSuccessRate path
                    // Add a tiny random factor for tie breaking
                    let rand = rng.NextDouble() * 0.01
                    (cat, rate + rand)
                )
                
            sortedCandidates |> List.truncate 10


