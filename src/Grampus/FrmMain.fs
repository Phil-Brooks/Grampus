namespace GrampusUI

open System.Windows.Forms
open System.Drawing
open Grampus

type FrmMain() as this =
    inherit Form(Text = "Grampus", WindowState = FormWindowState.Maximized, 
                    IsMdiContainer = true, Icon = Assets.Grampus)
    // --- Status Bar Labels (Stored as members to be updated) ---
    let lblStatus = new ToolStripStatusLabel(Text = "Ready")
    let lblEngine = new ToolStripStatusLabel(Text = "Engine: Idle", BorderSides = ToolStripStatusLabelBorderSides.Left, Margin = Padding(20, 0, 0, 0))
    let lblPosition = new ToolStripStatusLabel(Text = "", BorderSides = ToolStripStatusLabelBorderSides.Left)
    // --- Controls ---
    let bd = new PnlBoard(Dock = DockStyle.Top, Height = 600)
    let mh = new MoveHistoryPanel(Dock = DockStyle.Fill)
    let ap = new EngineAnalysisPanel(Dock = DockStyle.Top, Height = 100)
    let mr = new MasterDatabasePanel(Dock = DockStyle.Fill)
    let rep = new RepertoirePanel(Dock = DockStyle.Fill)
    let mutable currentRep = Repertoire.load repfol WHITE
    let mutable currentMode = Read
    let refreshRepTree() =
        rep.UpdateFullTree(currentRep)    
    let switchRep (side: int) =
        if currentMode = Edit then Repertoire.save repfol currentRep
        currentRep <- Repertoire.load repfol side
        bd.Orient(side) 
        mh.Clear()
        bd.SetBoard(Board.Start)
        refreshRepTree() 
        lblStatus.Text <- sprintf "Studying %s Repertoire" (if side = WHITE then "White" else "Black")
    // 2. Setup the Engine logic
    let onEngineMsg = function
        | Info info -> ap.UpdateAnalysis(info)
        | BestMove m -> printfn "Engine suggests: %s" m
        | Ready -> printfn "Engine is ready"
    let engine = Engine.spawn engloc onEngineMsg
    let updateAllowedMoves(history: Mv list) =
        if currentMode = Read then
            match Repertoire.findCurrentBranch currentRep.Roots history with
            | Some nodes -> bd.SetAllowedMoves(nodes |> List.map (fun n -> n.Mv))
            | None -> bd.SetAllowedMoves([]) // No moves allowed if off-book in Read mode
        else
            bd.SetAllowedMoves([]) // Ignore in Edit mode
    let setMode mode =
        currentMode <- mode
        bd.Mode <- mode
        // Disable comment editing in Read mode
        rep.SetMode (mode) // You'll need to add this method to RepertoirePanel
        let history = mh.GetMoveList()
        updateAllowedMoves(history)
        lblStatus.Text <- sprintf "Mode: %A | Studying %s" mode (if currentRep.Side = WHITE then "White" else "Black")
    // --- Menus ---
    let createMenu() =
        let ms = new MenuStrip()
        // File Menu
        let mnuFile = new ToolStripMenuItem("&File")
        // Dynamic Load Old Version Menu
        let mnuLoadBackup = new ToolStripMenuItem("&Load Old Version")
        // This event fires every time the "Load Old Version" sub-menu is hovered/clicked
        mnuLoadBackup.DropDownOpening.Add(fun _ ->
            mnuLoadBackup.DropDownItems.Clear()
            let backups = Repertoire.getVersions repfol currentRep.Side
            
            if backups.IsEmpty then
                let itmNone = new ToolStripMenuItem("No backups found")
                itmNone.Enabled <- false
                mnuLoadBackup.DropDownItems.Add(itmNone) |> ignore
            else
                // Create a menu item for each backup file
                for path in backups do
                    let fileInfo = System.IO.FileInfo(path)
                    let dateStr = fileInfo.LastWriteTime.ToString("yyyy-MM-dd HH:mm")
                    
                    let itm = new ToolStripMenuItem(sprintf "Backup: %s" dateStr, null, fun _ _ ->
                        // 1. Load the historical file
                        currentRep <- Repertoire.loadFromFile path currentRep.Side
                        
                        // 2. Reset the current game state to match the loaded file
                        bd.SetBoard(Board.Start)
                        mh.Clear()
                        ap.Clear()
                        refreshRepTree()
                        
                        lblStatus.Text <- sprintf "Restored version from %s" dateStr
                    )
                    mnuLoadBackup.DropDownItems.Add(itm) |> ignore
        )
        let itmExit = new ToolStripMenuItem("E&xit", null, (fun _ _ -> this.Close()))
        mnuFile.DropDownItems.Add(mnuLoadBackup) |> ignore
        mnuFile.DropDownItems.Add(new ToolStripSeparator()) |> ignore
        mnuFile.DropDownItems.Add(itmExit) |> ignore

        let mnuMode = new ToolStripMenuItem("&Mode")
        let itmEdit = new ToolStripMenuItem("Edit Mode (Build Repertoire)", null, fun _ _ -> setMode Edit)
        let itmRead = new ToolStripMenuItem("Read Mode (Practice)", null, fun _ _ -> setMode Read)
        mnuMode.DropDownItems.AddRange([| itmEdit :> ToolStripItem; itmRead |])

        // Study Menu (remains the same)
        let mnuStudy = new ToolStripMenuItem("&Study")
        let itmWhite = new ToolStripMenuItem("White Repertoire", Assets.White, (fun _ _ -> switchRep WHITE))
        let itmBlack = new ToolStripMenuItem("Black Repertoire", Assets.Black, (fun _ _ -> switchRep BLACK))
        let itmSave = new ToolStripMenuItem("&Save Now", Assets.Sav, fun _ _ -> 
                if currentMode = Edit then Repertoire.save repfol currentRep 
                else MessageBox.Show("Cannot save in Read Mode") |> ignore)
        mnuStudy.DropDownItems.AddRange([| itmWhite :> ToolStripItem; itmBlack :> ToolStripItem; new ToolStripSeparator() :> ToolStripItem; itmSave |])

        // Settings (remains the same)
        let mnuSettings = new ToolStripMenuItem("&Settings")
        let itmEngine = new ToolStripMenuItem("Set Engine Path...", null, fun _ _ ->
            let fd = new OpenFileDialog(Filter = "Executables|*.exe")
            if fd.ShowDialog() = DialogResult.OK then
                Settings.EngineLocation <- fd.FileName
                ConfigManager.save Settings
        )
        let mnuPieces = new ToolStripMenuItem("Piece Set")
        let addPieceOption (name: string) =
            let itm = new ToolStripMenuItem(name, null, fun _ _ ->
                Settings.PieceSet <- name
                ConfigManager.save Settings
                uipcs <- name
                Assets.Resest()
                bd.Redraw()
            )
            mnuPieces.DropDownItems.Add(itm) |> ignore
        ["Merida"; "Cburnett"; "Horsey"] |> List.iter addPieceOption
        
        let mnuThemes = new ToolStripMenuItem("Board Theme")
        let themes = [
            "Green", [Color.Green; Color.PaleGreen; Color.YellowGreen; Color.Yellow]
            "Red",   [Color.Red; Color.Pink; Color.PaleVioletRed; Color.HotPink]
        ]
        themes |> List.iter (fun (name, colors) ->
            let itm = new ToolStripMenuItem(name, null, fun _ _ ->
                Settings.ThemeColors <- colors |> List.map (fun c -> c.ToArgb())
                ConfigManager.save Settings
                uisqs <- colors
                bd.Redraw()
            )
            mnuThemes.DropDownItems.Add(itm) |> ignore
        )
        
        mnuSettings.DropDownItems.AddRange([| 
            itmEngine :> ToolStripItem
            new ToolStripSeparator() :> ToolStripItem
            mnuPieces :> ToolStripItem
            mnuThemes :> ToolStripItem
        |])

        ms.Items.Add(mnuFile) |> ignore
        ms.Items.Add(mnuMode) |> ignore
        ms.Items.Add(mnuStudy) |> ignore
        ms.Items.Add(mnuSettings) |> ignore
        ms   
    // --- Status Bar ---
    let createStatusBar() =
        let ss = new StatusStrip()
        ss.Items.AddRange([| lblStatus :> ToolStripItem; lblEngine; lblPosition |])
        ss
    let createToolbar() =
        let ts = new ToolStrip()
        let btnWhite = new ToolStripButton(Text = "Study White", CheckOnClick = true, Checked = true, Image = Assets.White)
        let btnBlack = new ToolStripButton(Text = "Study Black", CheckOnClick = true, Image = Assets.Black)
        btnWhite.Click.Add(fun _ -> 
            btnBlack.Checked <- false
            switchRep WHITE
        )
        btnBlack.Click.Add(fun _ -> 
            btnWhite.Checked <- false
            switchRep BLACK
        )
        let btnSave = new ToolStripButton(Text = "Save Changes", Image = Assets.Sav)
        btnSave.Click.Add(fun _ -> if currentMode = Edit then Repertoire.save repfol currentRep 
                                   else MessageBox.Show("Cannot save in Read Mode") |> ignore)
        ts.Items.Add(btnWhite) |> ignore
        ts.Items.Add(btnBlack) |> ignore
        ts.Items.Add(new ToolStripSeparator()) |> ignore
        ts.Items.Add(btnSave) |> ignore
        ts    
    let colHistory = new Panel(Dock = DockStyle.Left, Width = 184, BorderStyle = BorderStyle.FixedSingle)
    let colBoard   = new Panel(Dock = DockStyle.Left, Width = 600, BorderStyle = BorderStyle.FixedSingle)
    let colAnalysis = new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.FixedSingle)
    do 
        let menu = createMenu()
        let toolbar = createToolbar()
        let status = createStatusBar()
        this.MainMenuStrip <- menu
        colHistory.Controls.Add(mh)
        colBoard.Controls.Add(mr) 
        colBoard.Controls.Add(bd)
        colAnalysis.Controls.Add(rep)
        colAnalysis.Controls.Add(ap)
        this.Controls.Add(colAnalysis) 
        this.Controls.Add(colBoard)    
        this.Controls.Add(colHistory)  
        this.Controls.Add(toolbar)
        this.Controls.Add(menu)
        this.Controls.Add(status)
        // --- Event Wiring ---
        rep.OnMovesSelected.Add(fun moves ->
            // 1. Reset Board and History UI
            let mutable tempBoard = Board.Start
            mh.Clear()
            // 2. Play through the sequence to rebuild history and board state
            for m in moves do
                let bdBefore = tempBoard
                let san = San.ToSan bdBefore m
                mh.AddMove(bdBefore, m)
                tempBoard <- Board.MoveApply m tempBoard
            // 3. Set the final board position
            bd.SetBoard(tempBoard)
            // 4. Trigger analysis/Lichess for the new position
            let fen = FEN.FromBrd tempBoard
            lblPosition.Text <- sprintf "FEN: %s" (if fen.Length > 30 then fen.Substring(0, 27) + "..." else fen)
            ap.SetBoard(tempBoard)
            ap.Clear()
            engine.Post (SetPosition fen)
            engine.Post (StartSearch 10000)
            async {
                let! data = LichessClient.fetchMastersStats fen
                match data with | Some d -> mr.UpdateData(d) | None -> ()
            } |> Async.Start
        )
        bd.OnMoveMade.Add(fun (bdBefore, m) -> 
            let oldHistory = mh.GetMoveList()
            let san = San.ToSan bdBefore m
            if currentMode = Edit then
                currentRep <- Repertoire.update currentRep oldHistory m san
                refreshRepTree()
            mh.AddMove(bdBefore, m)
            updateAllowedMoves(mh.GetMoveList())
            let currentBrd = bd.GetBoard()
            let fen = FEN.FromBrd currentBrd
            lblStatus.Text <- sprintf "Last move: %s" san
            lblPosition.Text <- sprintf "FEN: %s" (if fen.Length > 30 then fen.Substring(0, 27) + "..." else fen)
            ap.SetBoard(currentBrd)
            ap.Clear()
            engine.Post (SetPosition fen)
            engine.Post (StartSearch 10000)
            async {
                let! data = LichessClient.fetchMastersStats fen
                match data with | Some d -> mr.UpdateData(d) | None -> ()
            } |> Async.Start
        )
        mh.OnMoveSelected.Add(fun moves ->
            let mutable tempBoard = Board.Start
            mh.Clear()
            for m in moves do
                let bdBefore = tempBoard
                mh.AddMove(bdBefore, m)
                tempBoard <- Board.MoveApply m tempBoard
            bd.SetBoard(tempBoard)
            updateAllowedMoves(moves)
            let fen = FEN.FromBrd tempBoard
            lblPosition.Text <- sprintf "FEN: %s" (if fen.Length > 30 then fen.Substring(0, 27) + "..." else fen)
            ap.SetBoard(tempBoard)
            ap.Clear()
            engine.Post (SetPosition fen)
            engine.Post (StartSearch 10000)
            async {
                let! data = LichessClient.fetchMastersStats fen
                match data with | Some d -> mr.UpdateData(d) | None -> ()
            } |> Async.Start
        )
        rep.OnCommentUpdated.Add(fun (node, newComment) ->
            // Update the immutable state
            currentRep <- Repertoire.setComment currentRep node newComment
            // Save immediately
            Repertoire.save repfol currentRep
            // Refresh tree to keep the 'Tag' data in the UI in sync with the record
            refreshRepTree()
        )    

        currentRep <- Repertoire.load repfol WHITE
        bd.Orient(WHITE)
        refreshRepTree()
        lblStatus.Text <- "Studying White Repertoire"
    
    override this.OnFormClosing(e) =
        engine.Post Quit
        base.OnFormClosing(e)
        if currentMode = Edit then Repertoire.save repfol currentRep