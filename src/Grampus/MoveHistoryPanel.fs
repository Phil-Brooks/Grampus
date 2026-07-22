namespace GrampusUI

open System.Drawing
open System.Windows.Forms
open Grampus

type MoveHistoryPanel() as this =
    inherit UserControl()
    
    let mutable history = HistoryLogic.emptyHistory
    let moveSelectedEvent = Event<Mv list>()
    let grid = new DataGridView(
        Dock = DockStyle.Fill, 
        AllowUserToAddRows = false,
        ReadOnly = true,
        RowHeadersVisible = false,
        BackgroundColor = Color.White,
        SelectionMode = DataGridViewSelectionMode.FullRowSelect
    )
    let refreshGrid (moves: Mv list) =
        grid.Rows.Clear()
        let mutable tempBoard = Board.Start
        moves |> List.iter (fun m ->
            let san = San.ToSan tempBoard m
            // Use your existing logic to determine if we add a new row or update the "Black" column
            let actions = HistoryLogic.getRequiredActions tempBoard san grid.Rows.Count
            actions |> List.iter (fun action ->
                match action with
                | AddNewRow(num, whiteSan) ->
                    grid.Rows.Add([| box num; box whiteSan; box "" |]) |> ignore
                | UpdateExistingRow(blackSan) ->
                    let lastRow = grid.Rows.[grid.Rows.Count - 1]
                    lastRow.Cells.[2].Value <- blackSan
            )
            // Apply move to get the board state for the NEXT move's SAN calculation
            tempBoard <- Board.MoveApply m tempBoard
        )
        if grid.RowCount > 0 then
            grid.FirstDisplayedScrollingRowIndex <- grid.RowCount - 1



    do
        grid.DefaultCellStyle.SelectionBackColor <- Color.White
        grid.DefaultCellStyle.SelectionForeColor <- Color.Black
        grid.ColumnHeadersDefaultCellStyle.SelectionBackColor <- Color.White
        grid.ColumnHeadersDefaultCellStyle.SelectionForeColor <- Color.Black

        grid.Columns.Add("No", "#") |> ignore
        grid.Columns.Add("White", "White") |> ignore
        grid.Columns.Add("Black", "Black") |> ignore
        
        // Formatting
        grid.Columns.[0].Width <- 24
        grid.Columns.[1].Width <- 80
        grid.Columns.[2].Width <- 80
        grid.CellDoubleClick.Add(fun e ->
            if e.RowIndex >= 0 && (e.ColumnIndex = 1 || e.ColumnIndex = 2) then
                let cellValue = grid.[e.ColumnIndex, e.RowIndex].Value
                if cellValue <> null && cellValue.ToString() <> "" then
                    // Calculate move index: Row 0/Col 1 -> 0, Row 0/Col 2 -> 1, Row 1/Col 1 -> 2...
                    let moveIndex = (e.RowIndex * 2) + (e.ColumnIndex - 1)
                    let moves = history.PlayedMoves
                    if moveIndex < moves.Length then
                        let truncMoves = moves |> List.take (moveIndex + 1)
                        history <-  { history with PlayedMoves = truncMoves }
                        refreshGrid truncMoves
                        moveSelectedEvent.Trigger(truncMoves)
        )
          
        this.Controls.Add(grid)

    member this.AddMove(bdBefore: Brd, m: Mv) =
        history <- HistoryLogic.addMoveToHistory m history
        let san = San.ToSan bdBefore m
        let actions = HistoryLogic.getRequiredActions bdBefore san grid.Rows.Count
        actions |> List.iter (fun action ->
            match action with
            | AddNewRow(num, whiteSan) ->
                grid.Rows.Add([| box num; box whiteSan; box "" |]) |> ignore
            | UpdateExistingRow(blackSan) ->
                let lastRow = grid.Rows.[grid.Rows.Count - 1]
                lastRow.Cells.[2].Value <- blackSan
        )
        grid.FirstDisplayedScrollingRowIndex <- grid.RowCount - 1
    member this.GetMoveList() = history.PlayedMoves
    member this.Clear() =
        history <- HistoryLogic.emptyHistory
        grid.Rows.Clear()
    [<CLIEvent>] 
    member this.OnMoveSelected = moveSelectedEvent.Publish
