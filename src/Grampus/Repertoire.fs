namespace Grampus

#if INTERACTIVE
#else
open System
open System.Text.Json
open System.Text.Json.Serialization
open System.IO
#endif

    type RepertoireNode = {
        Mv : Mv
        San : string
        Comment : string
        Path : Mv list
        Replies : RepertoireNode list
    }

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
            { Mv = move; San = san; Comment = ""; Path = [move]; Replies = [] }

        let rec private isPrefix (a: 'a list) (b: 'a list) =
            match a, b with
            | [], _ -> true
            | _, [] -> false
            | x :: xs, y :: ys -> x = y && isPrefix xs ys

        let update (repertoire: Repertoire) (history: Mv list) (newMv: Mv) (newSan: string) =
            let currentTurn = if history.Length % 2 = 0 then WHITE else BLACK
            let newPath = history @ [newMv]

            let filteredLines =
                if currentTurn = repertoire.Side then
                    // OUR SIDE (Single path rule)
                    repertoire.Lines |> List.filter (fun line ->
                        not (isPrefix history line && line.Length > history.Length && line.[history.Length] <> newMv)
                    )
                else
                    // OPPONENT SIDE (Variations rule)
                    repertoire.Lines

            let finalLines =
                let exists = filteredLines |> List.exists (fun line -> isPrefix newPath line)
                if exists then filteredLines
                else filteredLines @ [newPath]

            { repertoire with Lines = finalLines }

        let setComment (repertoire: Repertoire) (node: RepertoireNode) (comment: string) =
            let pathExists = repertoire.Lines |> List.exists (fun line -> isPrefix node.Path line)
            if not pathExists then repertoire
            else
                let newComments = Map.add node.Path comment repertoire.Comments
                { repertoire with Comments = newComments }

        let toTree (lines: Mv list list) (comments: Map<Mv list, string>) : RepertoireNode list =
            let rec build (bd: Brd) (currentPath: Mv list) (remainingLines: Mv list list) : RepertoireNode list =
                let activeLines = remainingLines |> List.filter (fun l -> not l.IsEmpty)
                if activeLines.IsEmpty then []
                else
                    activeLines
                    |> List.groupBy List.head
                    |> List.map (fun (mv, group) ->
                        let nextPath = currentPath @ [mv]
                        let nextBd = Board.MoveApply mv bd
                        let san = San.ToSan bd mv
                        let comment = Map.tryFind nextPath comments |> Option.defaultValue ""
                        
                        let nextRemaining = group |> List.map List.tail
                        let replies = build nextBd nextPath nextRemaining
                        
                        { Mv = mv; San = san; Comment = comment; Path = nextPath; Replies = replies }
                    )
            build Board.Start [] lines

        let ofTree name side (roots: RepertoireNode list) : Repertoire =
            let rec traverse (currentPath: Mv list) (nodes: RepertoireNode list) (accLines: Mv list list) (accComments: Map<Mv list, string>) =
                if nodes.IsEmpty then 
                    if currentPath.IsEmpty then (accLines, accComments)
                    else (currentPath :: accLines, accComments)
                else
                    nodes |> List.fold (fun (lines, comments) node ->
                        let nextPath = currentPath @ [node.Mv]
                        let updatedComments = 
                            if String.IsNullOrEmpty(node.Comment) then comments 
                            else Map.add nextPath node.Comment comments
                        
                        if node.Replies.IsEmpty then
                            (nextPath :: lines, updatedComments)
                        else
                            traverse nextPath node.Replies lines updatedComments
                    ) (accLines, accComments)
            
            let lines, comments = traverse [] roots [] Map.empty
            { Name = name; Side = side; Lines = List.rev lines; Comments = comments }

    type Repertoire with
        member this.Roots = Repertoire.toTree this.Lines this.Comments