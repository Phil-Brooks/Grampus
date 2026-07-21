namespace Grampus

type HistoryAction =
    | AddNewRow of moveNum:int * whiteSan:string
    | UpdateExistingRow of blackSan:string

type GameHistory = { PlayedMoves : Mv list }

module HistoryLogic =
    
    let emptyHistory = { PlayedMoves = [] }
    
    let addMoveToHistory (m: Mv) (history: GameHistory) =
        { PlayedMoves = history.PlayedMoves @ [m] }

    // Your existing logic remains the same
    let getRequiredActions (bdBefore: Brd) (san: string) (rowCount: int) : HistoryAction list =
        if bdBefore.WhosTurn = 0 then 
            [ AddNewRow(bdBefore.Fullmove, san) ]
        else 
            if rowCount = 0 then
                [ AddNewRow(bdBefore.Fullmove, "..."); UpdateExistingRow(san) ]
            else
                [ UpdateExistingRow(san) ]