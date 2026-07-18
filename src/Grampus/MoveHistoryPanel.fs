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

    member this.AddMove(bdBefore: Brd, m: Move) =
        let san = San.ToSan bdBefore m
        
        if bdBefore.WhosTurn = 0 then // White's turn
            let moveNum = bdBefore.Fullmove
            grid.Rows.Add([| box moveNum; box san; box "" |]) |> ignore
        else // Black's turn
            // Find the last row to update the Black move columns
            let lastRow = grid.Rows.[grid.Rows.Count - 1]
            lastRow.Cells.[2].Value <- san
            
        grid.FirstDisplayedScrollingRowIndex <- grid.RowCount - 1