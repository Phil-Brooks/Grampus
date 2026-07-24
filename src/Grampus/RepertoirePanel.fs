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

    // Event definitions
    let movesSelected = new Event<Mv list>()
    let commentUpdated = new Event<Mv list * string>()

    // Layout panels
    let layout = new TableLayoutPanel(
        Dock = DockStyle.Fill,
        RowCount = 3,
        ColumnCount = 1
    )

    let pnlNextMoves = new FlowLayoutPanel(
        Dock = DockStyle.Fill,
        FlowDirection = FlowDirection.LeftToRight,
        WrapContents = false,
        AutoScroll = true
    )

    let gridLines = new DataGridView(
        Dock = DockStyle.Fill,
        AllowUserToAddRows = false,
        ReadOnly = true,
        RowHeadersVisible = false,
        BackgroundColor = Color.White,
        SelectionMode = DataGridViewSelectionMode.CellSelect,
        EnableHeadersVisualStyles = false,
        MultiSelect = false
    )

    let txtComment = new TextBox(
        Multiline = true,
        ReadOnly = true,
        Dock = DockStyle.Fill,
        ScrollBars = ScrollBars.Vertical,
        BackColor = Color.LightGray,
        Font = new Font("Segoe UI", 10.0f)
    )

    let commentHeader = new Label(
        Text = "Comment:",
        Dock = DockStyle.Top,
        Height = 20
    )

    let pnlComment = new Panel(
        Dock = DockStyle.Fill
    )

    do
        // Setup TableLayoutPanel rows
        layout.RowStyles.Add(new RowStyle(SizeType.Absolute, 40.0f)) |> ignore
        layout.RowStyles.Add(new RowStyle(SizeType.Percent, 100.0f)) |> ignore
        layout.RowStyles.Add(new RowStyle(SizeType.Absolute, 100.0f)) |> ignore

        // Add controls to layout
        layout.Controls.Add(pnlNextMoves, 0, 0)
        layout.Controls.Add(gridLines, 0, 1)
        layout.Controls.Add(pnlComment, 0, 2)

        // Setup bottom comment panel
        pnlComment.Controls.Add(txtComment)
        pnlComment.Controls.Add(commentHeader)

        // Setup Grid styling
        gridLines.RowTemplate.Height <- 26
        gridLines.AllowUserToResizeRows <- false
        gridLines.ColumnHeadersDefaultCellStyle.BackColor <- Color.White
        gridLines.DefaultCellStyle.SelectionBackColor <- Color.AliceBlue
        gridLines.DefaultCellStyle.SelectionForeColor <- Color.Black
        gridLines.ColumnHeadersDefaultCellStyle.SelectionBackColor <- Color.White
        gridLines.ColumnHeadersDefaultCellStyle.SelectionForeColor <- Color.Black

        this.Controls.Add(layout)

        // Cell Double-Click -> Play moves up to this ply in the selected variation
        gridLines.CellDoubleClick.Add(fun e ->
            if e.RowIndex >= 0 && e.ColumnIndex > 0 then
                match currentRepertoire with
                | Some rep ->
                    let historyLength = currentHistory.Length
                    let filteredLines =
                        rep.Lines
                        |> List.filter (fun line ->
                            let rec isPrefix a b =
                                match a, b with
                                | [], _ -> true
                                | _, [] -> false
                                | x::xs, y::ys -> x = y && isPrefix xs ys
                            isPrefix currentHistory line
                        )
                    let lineIndex = e.ColumnIndex - 1
                    if lineIndex < filteredLines.Length then
                        let line = filteredLines.[lineIndex]
                        let prefixLen =
                            if historyLength % 2 = 0 then
                                historyLength + 2 * e.RowIndex + 2
                            else
                                if e.RowIndex = 0 then historyLength + 1
                                else historyLength + 2 * e.RowIndex + 1
                        let movesToPlay = line |> List.truncate prefixLen
                        movesSelected.Trigger(movesToPlay)
                | None -> ()
        )

        // Comment focus lost -> update comment for currentHistory
        txtComment.LostFocus.Add(fun _ ->
            match currentRepertoire with
            | Some rep ->
                let oldComment = Map.tryFind currentHistory rep.Comments |> Option.defaultValue ""
                if oldComment <> txtComment.Text then
                    let lastMv = if currentHistory.IsEmpty then { From=0; To=0; Pc=0; CapPc=0; Prom=0 } else List.last currentHistory
                    commentUpdated.Trigger(currentHistory, txtComment.Text)
            | None -> ()
        )

    // Helper to get all SAN figurine moves for a line
    let getSanList (line: Mv list) : string list =
        let rec helper bd moves acc =
            match moves with
            | [] -> List.rev acc
            | m :: rest ->
                let san = San.ToFigurine (San.ToSan bd m)
                let nextBd = Board.MoveApply m bd
                helper nextBd rest (san :: acc)
        helper Board.Start line []

    let rec isPrefix (a: 'a list) (b: 'a list) =
        match a, b with
        | [], _ -> true
        | _, [] -> false
        | x :: xs, y :: ys -> x = y && isPrefix xs ys

    [<CLIEvent>] member this.OnMovesSelected = movesSelected.Publish
    [<CLIEvent>] member this.OnCommentUpdated = commentUpdated.Publish

    member this.UpdateFullTree(repertoire: Repertoire, history: Mv list) =
        let updateAction() =
            currentHistory <- history
            currentRepertoire <- Some repertoire

            // 1. Update Top Panel: next moves
            pnlNextMoves.SuspendLayout()
            pnlNextMoves.Controls.Clear()

            let currentBd = history |> List.fold (fun b m -> Board.MoveApply m b) Board.Start
            let nextMoves =
                repertoire.Lines
                |> List.filter (fun line -> isPrefix history line && line.Length > history.Length)
                |> List.map (fun line -> line.[history.Length])
                |> List.distinct

            for m in nextMoves do
                let san = San.ToFigurine (San.ToSan currentBd m)
                let btn = new Button(
                    Text = san,
                    AutoSize = true,
                    Margin = new Padding(3, 3, 3, 3),
                    Font = new Font("Segoe UI Symbol", 9.0f)
                )
                btn.Click.Add(fun _ ->
                    movesSelected.Trigger(history @ [m])
                )
                pnlNextMoves.Controls.Add(btn)

            pnlNextMoves.ResumeLayout()

            // 2. Update Grid: side-by-side variations (scoresheet layout)
            gridLines.SuspendLayout()
            gridLines.Rows.Clear()
            gridLines.Columns.Clear()

            // Column 0: Move number
            gridLines.Columns.Add("#", "#") |> ignore
            gridLines.Columns.[0].Width <- 35
            gridLines.Columns.[0].DefaultCellStyle.Alignment <- DataGridViewContentAlignment.MiddleCenter
            gridLines.Columns.[0].SortMode <- DataGridViewColumnSortMode.NotSortable

            let filteredLines =
                repertoire.Lines
                |> List.filter (fun line -> isPrefix history line)

            // Add columns for each filtered line
            for i = 1 to filteredLines.Length do
                let colName = sprintf "Line %d" i
                gridLines.Columns.Add(colName, colName) |> ignore
                gridLines.Columns.[i].AutoSizeMode <- DataGridViewAutoSizeColumnMode.Fill
                gridLines.Columns.[i].SortMode <- DataGridViewColumnSortMode.NotSortable

            if not filteredLines.IsEmpty then
                // Pre-generate SAN list for each line
                let lineSanLists = filteredLines |> List.map getSanList

                // Calculate max plies remaining
                let maxPliesRemaining = 
                    filteredLines 
                    |> List.map (fun line -> line.Length - history.Length)
                    |> List.max

                // Calculate number of rows required
                let numRows = 
                    if history.Length % 2 = 0 then
                        (maxPliesRemaining + 1) / 2
                    else
                        if maxPliesRemaining <= 1 then 1
                        else 1 + (maxPliesRemaining - 1 + 1) / 2

                let startMoveNum = history.Length / 2 + 1

                for r = 0 to numRows - 1 do
                    let moveNum = startMoveNum + r
                    let rowData : obj[] = Array.create (filteredLines.Length + 1) null
                    rowData.[0] <- box moveNum

                    for lineIdx = 0 to filteredLines.Length - 1 do
                        let sanList = lineSanLists.[lineIdx]
                        let cellText =
                            if history.Length % 2 = 0 then
                                let wIdx = history.Length + 2 * r
                                let bIdx = history.Length + 2 * r + 1
                                let wSan = if wIdx < sanList.Length then sanList.[wIdx] else ""
                                let bSan = if bIdx < sanList.Length then sanList.[bIdx] else ""
                                if String.IsNullOrEmpty(wSan) then ""
                                elif String.IsNullOrEmpty(bSan) then wSan
                                else sprintf "%s %s" wSan bSan
                            else
                                if r = 0 then
                                    let bIdx = history.Length
                                    let bSan = if bIdx < sanList.Length then sanList.[bIdx] else ""
                                    if String.IsNullOrEmpty(bSan) then ""
                                    else sprintf "... %s" bSan
                                else
                                    let wIdx = history.Length + 2 * r - 1
                                    let bIdx = history.Length + 2 * r
                                    let wSan = if wIdx < sanList.Length then sanList.[wIdx] else ""
                                    let bSan = if bIdx < sanList.Length then sanList.[bIdx] else ""
                                    if String.IsNullOrEmpty(wSan) then ""
                                    elif String.IsNullOrEmpty(bSan) then wSan
                                    else sprintf "%s %s" wSan bSan
                        rowData.[lineIdx + 1] <- box cellText

                    gridLines.Rows.Add(rowData) |> ignore

            gridLines.ResumeLayout()

            // 3. Update Comments box directly to currentHistory comment
            let comment = Map.tryFind history repertoire.Comments |> Option.defaultValue ""
            txtComment.Text <- comment

        if this.IsHandleCreated then
            this.BeginInvoke(MethodInvoker(updateAction)) |> ignore
        else
            updateAction()

    member this.Clear() =
        currentHistory <- []
        currentRepertoire <- None
        if this.IsHandleCreated then
            this.BeginInvoke(MethodInvoker(fun () ->
                pnlNextMoves.Controls.Clear()
                gridLines.Rows.Clear()
                txtComment.Clear()
            )) |> ignore
        else
            pnlNextMoves.Controls.Clear()
            gridLines.Rows.Clear()
            txtComment.Clear()

    member this.SetMode(mode) =
        let isReadOnly = (mode = Read)
        txtComment.ReadOnly <- isReadOnly
        txtComment.BackColor <- if isReadOnly then Color.LightGray else Color.White