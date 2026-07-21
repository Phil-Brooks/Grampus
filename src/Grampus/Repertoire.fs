namespace Grampus

open System.Text.Json
open System.Text.Json.Serialization
open System.IO

    type MoveAnnotation = 
        | MainLine = 0   // The move you intend to play
        | Alternative = 1 // A secondary option
        | Opponent = 2   // A move the opponent might play
    type RepertoireNode = {
        Mv : Mv
        San : string
        Annotation : MoveAnnotation
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

        let private getFileName side = 
            if side = WHITE then "repertoire_white.json" else "repertoire_black.json"

        let save (repertoire: Repertoire) =
            let path = getFileName repertoire.Side
            let json = JsonSerializer.Serialize(repertoire, options)
            File.WriteAllText(path, json)

        let load (side: int) : Repertoire =
            let path = getFileName side
            if File.Exists(path) then
                try 
                    JsonSerializer.Deserialize<Repertoire>(File.ReadAllText(path), options)
                with _ -> { Name = "New Repertoire"; Side = side; Roots = [] }
            else 
                { Name = "New Repertoire"; Side = side; Roots = [] }

        /// Flips the board orientation if the repertoire is for Black
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
        let createNode move san annot =
            { Mv = move; San = san; Annotation = annot; Comment = ""; Replies = [] }