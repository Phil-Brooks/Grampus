namespace Grampus

type HistoryAction =
    | AddNewRow of moveNum:int * whiteSan:string
    | UpdateExistingRow of blackSan:string

module HistoryLogic =
    let getRequiredActions (bdBefore: Brd) (san: string) (rowCount: int) : HistoryAction list =
        if bdBefore.WhosTurn = 0 then // White's turn
            [ AddNewRow(bdBefore.Fullmove, san) ]
        else // Black's turn
            if rowCount = 0 then
                // Return TWO actions: first add the placeholder, then update it
                [ AddNewRow(bdBefore.Fullmove, "..."); UpdateExistingRow(san) ]
            else
                [ UpdateExistingRow(san) ]
