namespace GrampusUI

open System
open System.Drawing
open System.Windows.Forms
open Grampus

type EngineAnalysisPanel() as this =
    inherit UserControl()
    
    let mutable currentBoard : Brd option = None

    let grid = new DataGridView(
        Dock = DockStyle.Fill, 
        AllowUserToAddRows = false,
        ReadOnly = true,
        RowHeadersVisible = false,
        BackgroundColor = Color.White,
        BorderStyle = BorderStyle.None,
        SelectionMode = DataGridViewSelectionMode.FullRowSelect,
        AllowUserToResizeRows = false,
        AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells
    )

    do
        // UI Styling Improvements
        grid.DefaultCellStyle.SelectionBackColor <- Color.LightGray
        grid.DefaultCellStyle.SelectionForeColor <- Color.Black
        
        grid.Columns.Add("Depth", "D") |> ignore
        grid.Columns.Add("Score", "Score") |> ignore
        grid.Columns.Add("Nodes", "Nodes") |> ignore
        grid.Columns.Add("Pv", "Line") |> ignore
        
        grid.Columns.[0].Width <- 35
        grid.Columns.[1].Width <- 60
        grid.Columns.[2].Width <- 70
        
        // Wrap text for the "Line" column
        grid.Columns.[3].AutoSizeMode <- DataGridViewAutoSizeColumnMode.Fill
        grid.Columns.[3].DefaultCellStyle.WrapMode <- DataGridViewTriState.True
        
        this.Controls.Add(grid)

    member this.SetBoard(bd: Brd) = currentBoard <- Some bd

    member this.UpdateAnalysis(info: Analysis) =
        if this.InvokeRequired then
            this.Invoke(Action(fun () -> this.UpdateAnalysis(info))) |> ignore
        else
            // 1. Ensure we have enough rows in the grid
            while grid.Rows.Count < info.MultiPvIndex do
                grid.Rows.Add() |> ignore
            // 2. Target the specific row for this PV index (0-indexed)
            let row = grid.Rows.[info.MultiPvIndex - 1]
            row.Cells.[0].Value <- info.Depth
            row.Cells.[1].Value <- AnalysisDisplay.formatScore currentBoard.Value.WhosTurn info.Score
            row.Cells.[2].Value <- AnalysisDisplay.formatNodes info.Nodes
            row.Cells.[3].Value <- AnalysisDisplay.getSanPv currentBoard.Value info.Pv
            // 3. Optional: Color the top move slightly differently
            if info.MultiPvIndex = 1 then
                row.DefaultCellStyle.Font <- new Font(grid.Font, FontStyle.Bold)            
    member this.Clear() =
        if this.InvokeRequired then this.Invoke(Action(fun () -> grid.Rows.Clear())) |> ignore
        else grid.Rows.Clear()