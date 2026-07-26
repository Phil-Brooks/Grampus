namespace GrampusUI

open System
open System.Drawing
open System.Windows.Forms
open Grampus

type AppMode = Edit | Read

type RepertoirePanel() as this =
    inherit UserControl()

    let mutable currentHistory : Mv list = []
    let mutable currentRepertoire : Repertoire option = None

    let movesSelected = new Event<Mv list>()
    let commentUpdated = new Event<Mv list * string>()

    let layout = new TableLayoutPanel(Dock = DockStyle.Fill, RowCount = 3, ColumnCount = 1)
    let pnlNextMoves = new FlowLayoutPanel(Dock = DockStyle.Fill, FlowDirection = FlowDirection.LeftToRight, WrapContents = false, AutoScroll = true)
    
    let gridLines = new DataGridView(
        Dock = DockStyle.Fill,
        AllowUserToAddRows = false,
        ReadOnly = true,
        RowHeadersVisible = false,
        BackgroundColor = Color.White,
        SelectionMode = DataGridViewSelectionMode.CellSelect,
        EnableHeadersVisualStyles = false,
        MultiSelect = false,
        BorderStyle = BorderStyle.None
    )

    let txtComment = new TextBox(Multiline = true, ReadOnly = true, Dock = DockStyle.Fill, ScrollBars = ScrollBars.Vertical, BackColor = Color.LightGray, Font = new Font("Segoe UI", 10.0f))
    let commentHeader = new Label(Text = "Comment:", Dock = DockStyle.Top, Height = 20)
    let pnlComment = new Panel(Dock = DockStyle.Fill)

    do
        layout.RowStyles.Add(new RowStyle(SizeType.Absolute, 40.0f)) |> ignore
        layout.RowStyles.Add(new RowStyle(SizeType.Percent, 100.0f)) |> ignore
        layout.RowStyles.Add(new RowStyle(SizeType.Absolute, 100.0f)) |> ignore
        layout.Controls.Add(pnlNextMoves, 0, 0)
        layout.Controls.Add(gridLines, 0, 1)
        layout.Controls.Add(pnlComment, 0, 2)
        pnlComment.Controls.Add(txtComment)
        pnlComment.Controls.Add(commentHeader)

        gridLines.RowTemplate.Height <- 26
        gridLines.DefaultCellStyle.Font <- new Font("Segoe UI Symbol", 9.0f)
        gridLines.ColumnHeadersDefaultCellStyle.Font <- new Font("Segoe UI", 8.5f, FontStyle.Bold)

        this.Controls.Add(layout)

        // --- Handle Move Selection via Double Click ---
        gridLines.CellDoubleClick.Add(fun e ->
            if e.RowIndex >= 0 && e.ColumnIndex > 0 then
                match currentRepertoire with
                | Some rep ->
                    let lineIdx = (e.ColumnIndex - 1) / 2
                    let isWhiteCol = (e.ColumnIndex - 1) % 2 = 0
                    
                    let filteredLines = rep.Lines |> List.filter (Repertoire.IsPrefix currentHistory)
                    if lineIdx < filteredLines.Length then
                        let line = filteredLines.[lineIdx]
                        
                        // Calculate Ply Index
                        // Row 0 corresponds to the start move number
                        let hLen = currentHistory.Length
                        let plyIndex = 
                            if hLen % 2 = 0 then
                                (hLen + (e.RowIndex * 2) + (if isWhiteCol then 0 else 1))
                            else
                                (hLen + (e.RowIndex * 2) + (if isWhiteCol then -1 else 0))

                        if plyIndex >= 0 && plyIndex < line.Length then
                            let movesToPlay = line |> List.truncate (plyIndex + 1)
                            movesSelected.Trigger(movesToPlay)
                | None -> ()
        )

        txtComment.LostFocus.Add(fun _ ->
            match currentRepertoire with
            | Some rep ->
                let oldComment = Map.tryFind currentHistory rep.Comments |> Option.defaultValue ""
                if oldComment <> txtComment.Text then
                    commentUpdated.Trigger(currentHistory, txtComment.Text)
            | None -> ()
        )

    let getSanList (line: Mv list) : string list =
        let rec helper bd moves acc =
            match moves with
            | [] -> List.rev acc
            | m :: rest ->
                let san = San.ToFigurine (San.ToSan bd m)
                let nextBd = Board.MoveApply m bd
                helper nextBd rest (san :: acc)
        helper Board.Start line []

    member this.UpdateAll(repertoire: Repertoire, history: Mv list) =
        let updateAction() =
            currentHistory <- history
            currentRepertoire <- Some repertoire

            // 1. Next Moves Buttons
            pnlNextMoves.SuspendLayout()
            pnlNextMoves.Controls.Clear()
            let currentBd = history |> List.fold (fun b m -> Board.MoveApply m b) Board.Start
            let nextMoves =
                repertoire.Lines
                |> List.filter (fun line -> Repertoire.IsPrefix history line && line.Length > history.Length)
                |> List.map (fun line -> line.[history.Length])
                |> List.distinct
            for m in nextMoves do
                let btn = new Button(Text = San.ToFigurine (San.ToSan currentBd m), AutoSize = true, Font = new Font("Segoe UI Symbol", 9.0f))
                btn.Click.Add(fun _ -> movesSelected.Trigger(history @ [m]))
                pnlNextMoves.Controls.Add(btn)
            pnlNextMoves.ResumeLayout()

            // 2. Grid Update
            gridLines.SuspendLayout()
            gridLines.Rows.Clear()
            gridLines.Columns.Clear()

            gridLines.Columns.Add(new DataGridViewTextBoxColumn(Name="#", HeaderText="#", Width=35, ReadOnly=true)) |> ignore
            gridLines.Columns.[0].DefaultCellStyle.Alignment <- DataGridViewContentAlignment.MiddleCenter
            gridLines.Columns.[0].DefaultCellStyle.BackColor <- Color.WhiteSmoke

            let filteredLines = repertoire.Lines |> List.filter (Repertoire.IsPrefix history)
            
            // Create 2 columns per variation
            for i = 0 to filteredLines.Length - 1 do
                let variationColor = if i % 2 = 0 then Color.White else Color.FromArgb(245, 248, 255)
                
                let colW = new DataGridViewTextBoxColumn(HeaderText = sprintf "L%d W" (i+1), Width = 60)
                colW.DefaultCellStyle.BackColor <- variationColor
                gridLines.Columns.Add(colW) |> ignore
                
                let colB = new DataGridViewTextBoxColumn(HeaderText = sprintf "L%d B" (i+1), Width = 60)
                colB.DefaultCellStyle.BackColor <- variationColor
                gridLines.Columns.Add(colB) |> ignore

            if not filteredLines.IsEmpty then
                let lineSanLists = filteredLines |> List.map getSanList
                let maxPlies = filteredLines |> List.map (fun l -> l.Length) |> List.max
                let startPly = history.Length
                
                // Calculate move numbers to display
                // If history is [d4] (len 1), we are at move 1 Black. Row 0 is Move 1.
                let firstMoveNum = (startPly / 2) + 1
                let lastMoveNum = (maxPlies - 1) / 2 + 1
                
                for mNum = firstMoveNum to lastMoveNum do
                    let rowIndex = mNum - firstMoveNum
                    let rowData : obj[] = Array.create (gridLines.Columns.Count) null
                    rowData.[0] <- mNum

                    for lIdx = 0 to filteredLines.Length - 1 do
                        let sans = lineSanLists.[lIdx]
                        let plyW = (mNum - 1) * 2
                        let plyB = plyW + 1
                        
                        // Assign text for White column
                        if plyW >= startPly && plyW < sans.Length then
                            rowData.[1 + lIdx * 2] <- sans.[plyW]
                        elif plyW < startPly && plyB >= startPly then
                            rowData.[1 + lIdx * 2] <- "..." // Placeholder for starting mid-move

                        // Assign text for Black column
                        if plyB >= startPly && plyB < sans.Length then
                            rowData.[2 + lIdx * 2] <- sans.[plyB]

                    gridLines.Rows.Add(rowData) |> ignore

            gridLines.ResumeLayout()
            txtComment.Text <- Map.tryFind history repertoire.Comments |> Option.defaultValue ""

        if this.IsHandleCreated then this.BeginInvoke(MethodInvoker(updateAction)) |> ignore else updateAction()

    [<CLIEvent>] member this.OnMovesSelected = movesSelected.Publish
    [<CLIEvent>] member this.OnCommentUpdated = commentUpdated.Publish
    member this.SetMode(mode) =
        let isRead = (mode = Read)
        txtComment.ReadOnly <- isRead
        txtComment.BackColor <- if isRead then Color.LightGray else Color.White