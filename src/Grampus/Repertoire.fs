namespace Grampus

    type MoveAnnotation = 
        | MainLine    // The move you intend to play
        | Alternative // A secondary option
        | Opponent    // A move the opponent might play
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
                |> List.tryFind (fun n -> n.Mv.From = currentMove.From && n.Mv.To = currentMove.To)
                |> Option.bind (fun matchingNode -> 
                    if remainingMoves.IsEmpty then 
                        Some matchingNode.Replies 
                    else 
                        findCurrentBranch matchingNode.Replies remainingMoves)

        /// Helper to create a new move node
        let createNode move san annot =
            { Mv = move; San = san; Annotation = annot; Comment = ""; Replies = [] }