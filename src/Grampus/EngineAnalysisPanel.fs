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

    // Helper: Normalize score so (+) is White and (-) is Black
    let formatScore score =
        let sideToMove = match currentBoard with Some b -> b.WhosTurn | _ -> 0
        match score with
        | Centipawns cp -> 
            // In UCI, scores are relative to side-to-move. 
            // If it's Black's turn, -cp means White is winning.
            let normalized = if sideToMove = 1 then -cp else cp
            let val' = float normalized / 100.0
            if val' > 0.0 then sprintf "+%.2f" val' else sprintf "%.2f" val'
        | MateIn m -> 
            let normalized = if sideToMove = 1 then -m else m
            sprintf "#%d" normalized
        | Unknown -> "-"

    // Helper: Convert UCI list [e2e4, e7e5] -> SAN string "e4 e5"
    let getSanPv (moves: string list) =
        match currentBoard with
        | None -> String.concat " " moves
        | Some startBd ->
            let mutable tempBd = startBd
            let mutable moveNum = startBd.Fullmove
            let mutable isWhite = (startBd.WhosTurn = 0)
        
            let sanMoves = 
                moves |> List.choose (fun uci ->
                    match UciMove.fromString tempBd uci with
                    | Some m ->
                        let san = San.ToSan tempBd m
                        let prefix = if isWhite then sprintf "%d. %s" moveNum san else san
                    
                        // Update state for next move in string
                        tempBd <- Board.MoveApply m tempBd
                        if not isWhite then moveNum <- moveNum + 1
                        isWhite <- not isWhite
                    
                        Some prefix
                    | None -> None
                )
            String.concat " " sanMoves    

    let formatNodes (n: int64) =
        if n > 1_000_000L then sprintf "%.1fM" (float n / 1_000_000.0)
        elif n > 1_000L then sprintf "%.1fk" (float n / 1_000.0)
        else n.ToString()

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
            row.Cells.[1].Value <- formatScore info.Score
            row.Cells.[2].Value <- formatNodes info.Nodes
            row.Cells.[3].Value <- getSanPv info.Pv

            // 3. Optional: Color the top move slightly differently
            if info.MultiPvIndex = 1 then
                row.DefaultCellStyle.Font <- new Font(grid.Font, FontStyle.Bold)            
    member this.Clear() =
        if this.InvokeRequired then this.Invoke(Action(fun () -> grid.Rows.Clear())) |> ignore
        else grid.Rows.Clear()