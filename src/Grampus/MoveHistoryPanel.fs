namespace GrampusUI

open System.Drawing
open System.Windows.Forms
open Grampus

type MoveHistoryPanel() as this =
    inherit UserControl()
    
    let grid = new DataGridView(
        Dock = DockStyle.Fill, 
        AllowUserToAddRows = false,
        ReadOnly = true,
        RowHeadersVisible = false,
        BackgroundColor = Color.White,
        BorderStyle = BorderStyle.None,
        SelectionMode = DataGridViewSelectionMode.FullRowSelect
    )

    do
        grid.Columns.Add("No", "#") |> ignore
        grid.Columns.Add("White", "White") |> ignore
        grid.Columns.Add("Black", "Black") |> ignore
        
        // Formatting
        grid.Columns.[0].Width <- 30
        this.Controls.Add(grid)

    member this.AddMove(bdBefore: Brd, m: Mv) =
        let san = San.ToSan bdBefore m
        // Call the new logic that returns a list
        let actions = HistoryLogic.getRequiredActions bdBefore san grid.Rows.Count
        
        // Loop through each action and apply it to the grid
        actions |> List.iter (fun action ->
            match action with
            | AddNewRow(num, whiteSan) ->
                grid.Rows.Add([| box num; box whiteSan; box "" |]) |> ignore
            | UpdateExistingRow(blackSan) ->
                let lastRow = grid.Rows.[grid.Rows.Count - 1]
                lastRow.Cells.[2].Value <- blackSan
        )
            
        grid.FirstDisplayedScrollingRowIndex <- grid.RowCount - 1