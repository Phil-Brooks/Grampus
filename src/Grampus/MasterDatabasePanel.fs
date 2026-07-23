namespace GrampusUI

open System
open System.Drawing
open System.Windows.Forms
open Grampus

type MasterDatabasePanel() as this =
    inherit UserControl()
    
    // Create these once. Do NOT dispose them inside the paint loop.
    let barFont = new Font("Segoe UI", 7.5f)
    let centerFormat = new StringFormat(Alignment = StringAlignment.Center, LineAlignment = StringAlignment.Center)
    
    let grid = new DataGridView(
        Dock = DockStyle.Fill, AllowUserToAddRows = false, ReadOnly = true,
        RowHeadersVisible = false, BackgroundColor = Color.White,
        SelectionMode = DataGridViewSelectionMode.FullRowSelect,
        EnableHeadersVisualStyles = false,
        // Using standard DataGridView - no subclassing
        AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None
    )

    do
        grid.RowTemplate.Height <- 30
        grid.AllowUserToResizeRows <- false        
        grid.ColumnHeadersDefaultCellStyle.BackColor <- Color.White
        grid.DefaultCellStyle.SelectionBackColor <- Color.White
        grid.DefaultCellStyle.SelectionForeColor <- Color.Black
        grid.ColumnHeadersDefaultCellStyle.SelectionBackColor <- Color.White
        grid.ColumnHeadersDefaultCellStyle.SelectionForeColor <- Color.Black
        
        grid.Columns.Add("Move", "Move") |> ignore
        grid.Columns.Add("Games", "Games") |> ignore
        grid.Columns.Add("Results", "White / Draw / Black") |> ignore
        
        grid.Columns.[0].Width <- 55
        grid.Columns.[1].Width <- 65
        grid.Columns.[2].AutoSizeMode <- DataGridViewAutoSizeColumnMode.Fill

        grid.CellPainting.Add(fun e ->
            // Safety 1: Ensure valid row and positive bounds
            if e.RowIndex >= 0 && e.ColumnIndex >= 0 && e.CellBounds.Width > 0 then
                
                // Safety 2: Check for null values
                if e.Value = null then 
                    e.PaintBackground(e.CellBounds, true)
                    e.Handled <- true
                else
                    match e.ColumnIndex with
                    | 2 -> // THE BAR COLUMN
                        match e.Value with
                        | :? MasterDataLogic.WinLossRatios as ratios ->
                            // Paint background manually to handle selection
                            let isSelected = e.State.HasFlag(DataGridViewElementStates.Selected)
                            let backCol = if isSelected then e.CellStyle.SelectionBackColor else e.CellStyle.BackColor
                            use backBrush = new SolidBrush(backCol)
                            e.Graphics.FillRectangle(backBrush, e.CellBounds)

                            let b = e.CellBounds
                            // Simplified bar (No clipping to prevent GDI+ crashes)
                            let barW = float32 b.Width - 10f
                            let barH = float32 b.Height - 12f
                            let barX = float32 b.X + 5f
                            let barY = float32 b.Y + 6f

                            if barW > 10f then
                                let wW = barW * float32 ratios.WhitePct
                                let dW = barW * float32 ratios.DrawPct
                                let bW = barW - wW - dW

                                use wb = new SolidBrush(Color.FromArgb(242, 242, 242))
                                use db = new SolidBrush(Color.FromArgb(160, 160, 160))
                                use bb = new SolidBrush(Color.FromArgb(80, 80, 80))

                                e.Graphics.FillRectangle(wb, barX, barY, wW, barH)
                                e.Graphics.FillRectangle(db, barX + wW, barY, dW, barH)
                                e.Graphics.FillRectangle(bb, barX + wW + dW, barY, bW, barH)

                                // Labels
                                let drawL pct x w col =
                                    if w > 25f then
                                        use tb = new SolidBrush(col)
                                        let rect = RectangleF(x, barY, w, barH)
                                        e.Graphics.DrawString(sprintf "%.0f%%" (pct * 100.0), barFont, tb, rect, centerFormat)

                                drawL ratios.WhitePct barX wW Color.Black
                                drawL ratios.DrawPct (barX + wW) dW Color.White
                                drawL ratios.BlackPct (barX + wW + dW) bW Color.White
                            
                            e.Handled <- true
                        | _ -> ()
                    | _ -> 
                        // TEXT COLUMNS (Move, Games)
                        // Explicitly paint the background and text to prevent "Invisibility"
                        let isSelected = e.State.HasFlag(DataGridViewElementStates.Selected)
                        let backCol = if isSelected then e.CellStyle.SelectionBackColor else e.CellStyle.BackColor
                        let foreCol = if isSelected then e.CellStyle.SelectionForeColor else e.CellStyle.ForeColor
                        
                        use backBrush = new SolidBrush(backCol)
                        e.Graphics.FillRectangle(backBrush, e.CellBounds)
                        
                        let text = e.Value.ToString()
                        TextRenderer.DrawText(e.Graphics, text, e.CellStyle.Font, e.CellBounds, foreCol, TextFormatFlags.VerticalCenter ||| TextFormatFlags.Left)
                        e.Handled <- true
        )        
        this.Controls.Add(grid)

    member this.UpdateData(data: MasterResponse) =
        // Use BeginInvoke to prevent the UI thread from locking up
        this.BeginInvoke(MethodInvoker(fun () -> 
            try
                grid.SuspendLayout()
                grid.Rows.Clear()
            
                if data.Moves <> null then
                    for m in data.Moves do
                        let ratios = MasterDataLogic.calculateRatios m.White m.Draws m.Black
                        let total = m.White + m.Draws + m.Black
                        let san = San.ToFigurine m.San
                        grid.Rows.Add([| box san; box (MasterDataLogic.formatCount total); box ratios |]) |> ignore
            
                let totalRatios = MasterDataLogic.calculateRatios data.White data.Draws data.Black
                let totalGames = data.White + data.Draws + data.Black
                let sumRowIdx = grid.Rows.Add([| box "Σ"; box (MasterDataLogic.formatCount totalGames); box totalRatios |])
                grid.Rows.[sumRowIdx].DefaultCellStyle.BackColor <- Color.AliceBlue
                grid.Rows.[sumRowIdx].DefaultCellStyle.Font <- new Font(grid.Font, FontStyle.Bold)
            
                grid.ResumeLayout()
                // Force a full redraw
                grid.Refresh()
            with _ -> 
                if not grid.IsDisposed then grid.ResumeLayout()
        )) |> ignore

    override this.Dispose(disposing) =
        if disposing then
            barFont.Dispose()
            centerFormat.Dispose()
        base.Dispose(disposing)