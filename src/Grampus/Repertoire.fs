namespace Grampus

open System
open System.Text.Json
open System.Text.Json.Serialization
open System.IO

    type RepertoireNode = {
        Mv : Mv
        San : string
        Comment : string
        Replies : RepertoireNode list
    }
    type Repertoire = {
        Name : string
        Side : int // WHITE or BLACK constants from your Types module
        Roots : RepertoireNode list
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
            let json = JsonSerializer.Serialize(repertoire, options)
            File.WriteAllText(path, json)
        let loadFromFile path side =
            if File.Exists(path) then
                try 
                    JsonSerializer.Deserialize<Repertoire>(File.ReadAllText(path), options)
                with _ -> { Name = "New Repertoire"; Side = side; Roots = [] }
            else 
                { Name = "New Repertoire"; Side = side; Roots = [] }
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
            // If repertoire is for BLACK, return BLACK (1), else WHITE (0)
            repertoire.Side
        /// Finds the branch in the repertoire that matches a list of moves played
        /// movesPlayed: A list of Mv records from the start of the game
        let rec findCurrentBranch (nodes: RepertoireNode list) (movesPlayed: Mv list) =
            match movesPlayed with
            | [] -> Some nodes // We are at the root
            | currentMove :: remainingMoves ->
                // Look for a node in this level that matches the move played
                nodes 
                |> List.tryFind (fun n -> n.Mv.From = currentMove.From && n.Mv.To = currentMove.To && n.Mv.Prom = currentMove.Prom)
                |> Option.bind (fun matchingNode -> 
                    if remainingMoves.IsEmpty then 
                        Some matchingNode.Replies 
                    else 
                        findCurrentBranch matchingNode.Replies remainingMoves)
        /// Helper to create a new move node
        let createNode move san =
            { Mv = move; San = san; Comment = ""; Replies = [] }
        let rec private updateNodes (nodes: RepertoireNode list) (history: Mv list) (newMv: Mv) (newSan: string) (studySide: int) (currentTurn: int) =
            match history with
            | head :: tail ->
                // 1. Navigate deeper: find the branch that matches the history
                nodes |> List.map (fun node ->
                    if node.Mv = head then
                        // Flip the turn for the next level
                        let nextTurn = if currentTurn = WHITE then BLACK else WHITE
                        { node with Replies = updateNodes node.Replies tail newMv newSan studySide nextTurn }
                    else node)
            | [] ->
                // 2. We are at the position where the move is made. Apply Business Rules:
                if currentTurn = studySide then
                    // RULE: OUR SIDE (Single path)
                    let existing = nodes |> List.tryFind (fun n -> n.Mv = newMv)
                    match existing with
                    | Some found -> [ found ] // Already exists: keep only this one
                    | None -> [ { Mv = newMv; San = newSan; Comment = ""; Replies = [] } ] // Replace all with new move
                else
                    // RULE: OPPONENT SIDE (Variations)
                    let exists = nodes |> List.exists (fun n -> n.Mv = newMv)
                    if exists then nodes // Already exists: don't change anything
                    else nodes @ [ { Mv = newMv; San = newSan; Comment = ""; Replies = [] } ] // Add new variation
        /// Public entry point to update the repertoire structure
        let update (repertoire: Repertoire) (history: Mv list) (newMv: Mv) (newSan: string) =
            // Start recursion from the root. The first move of a game is always WHITE's turn.
            let newRoots = updateNodes repertoire.Roots history newMv newSan repertoire.Side WHITE
            { repertoire with Roots = newRoots }
        /// Recursively updates a comment for a specific move node in the tree
        let rec updateComment (nodes: RepertoireNode list) (targetMv: Mv) (newComment: string) =
            nodes |> List.map (fun node ->
                if node.Mv = targetMv then
                    { node with Comment = newComment }
                else
                    { node with Replies = updateComment node.Replies targetMv newComment }
            )
        let setComment (repertoire: Repertoire) (node: RepertoireNode) (comment: string) =
            { repertoire with Roots = updateComment repertoire.Roots node.Mv comment }