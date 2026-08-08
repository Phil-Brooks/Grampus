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
    let refreshRep() =
        rep.UpdateAll(currentRep, mh.GetMoveList())    
    let updateAllowedMoves(history: Mv list) =
        if currentMode = Read then
            let nextMoves =
                currentRep.Lines
                |> List.filter (fun line -> Repertoire.IsPrefix history line && line.Length > history.Length)
                |> List.map (fun line -> line.[history.Length])
                |> List.distinct
            bd.SetAllowedMoves(nextMoves)
        else
            bd.SetAllowedMoves([]) // Ignore in Edit mode
    let switchRep (side: int) =
        if currentMode = Edit then Repertoire.save repfol currentRep
        currentRep <- Repertoire.load repfol side
        bd.Orient(side) 
        mh.Clear()
        bd.SetBoard(Board.Start)
        refreshRep()
        updateAllowedMoves([])
        lblStatus.Text <- sprintf "Studying %s Repertoire" (if side = WHITE then "White" else "Black")
    // 2. Setup the Engine logic
    let onEngineMsg = function
        | Info info -> ap.UpdateAnalysis(info)
        | BestMove m -> printfn "Engine suggests: %s" m
        | Ready -> printfn "Engine is ready"
    let engine = Engine.spawn engloc onEngineMsg
    let setMode mode =
        currentMode <- mode
        bd.Mode <- mode
        rep.SetMode (mode)
        let history = mh.GetMoveList()
        updateAllowedMoves(history)
        lblStatus.Text <- sprintf "Mode: %A | Studying %s" mode (if currentRep.Side = WHITE then "White" else "Black")

    let mutable currentTrainingPositions : Mv list list = []
    let mutable trainingIndex = 0
    let mutable attemptsLeft = 3
    let mutable successCount = 0
    let mutable currentTrainingCorrectMoves : Mv list = []
    let mutable trainingStats : Map<Mv list, PositionStats> = Map.empty

    let rec loadTrainingPosition() =
        if trainingIndex >= currentTrainingPositions.Length then
            TrainingStats.save repfol currentRep.Side trainingStats
            MessageBox.Show(sprintf "Training Session Complete!\nScore: %d / %d" successCount currentTrainingPositions.Length, "Grampus Training") |> ignore
            setMode Read
            bd.SetBoard(Board.Start)
            mh.Clear()
            refreshRep()
        else
            let path = currentTrainingPositions.[trainingIndex]
            attemptsLeft <- 3
            currentTrainingCorrectMoves <- 
                currentRep.Lines
                |> List.filter (fun line -> line.Length > path.Length && Repertoire.IsPrefix path line)
                |> List.map (fun line -> line.[path.Length])
                |> List.distinct
            
            let mutable tempBoard = Board.Start
            mh.Clear()
            for mv in path do
                mh.AddMove(tempBoard, mv)
                tempBoard <- Board.MoveApply mv tempBoard
            bd.SetBoard(tempBoard)
            
            lblStatus.Text <- sprintf "Training: Position %d/%d | Attempts left: %d" (trainingIndex + 1) currentTrainingPositions.Length attemptsLeft
            ap.SetBoard(tempBoard)
            ap.Clear()
            let fen = FEN.FromBrd tempBoard
            lblPosition.Text <- sprintf "FEN: %s" (if fen.Length > 30 then fen.Substring(0, 27) + "..." else fen)
            async {
                let! data = LichessClient.fetchMastersStats fen
                match data with | Some d -> mr.UpdateData(d) | None -> ()
            } |> Async.Start

    let startTraining() =
        let startPath = mh.GetMoveList()
        trainingStats <- TrainingStats.load repfol currentRep.Side
        let selected = Training.selectTrainingPositions currentRep startPath trainingStats
        if selected.IsEmpty then
            MessageBox.Show("No training positions found from the current position/repertoire.", "Grampus Training") |> ignore
        else
            let untestedCount = selected |> List.filter (fun p -> not (Map.containsKey p trainingStats)) |> List.length
            currentTrainingPositions <- selected
            trainingIndex <- 0
            successCount <- 0
            setMode Train
            loadTrainingPosition()
            if untestedCount > 0 then
                lblStatus.Text <- sprintf "Training started! %d untested positions." untestedCount
    // --- Menus ---
    let createMenu() =
        let ms = new MenuStrip()
        // File Menu
        let mnuFile = new ToolStripMenuItem("&File")
        // Dynamic Load Old Version Menu
        let mnuLoadBackup = new ToolStripMenuItem("&Load Old Version", Assets.Old)
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
                        refreshRep()
                        
                        lblStatus.Text <- sprintf "Restored version from %s" dateStr
                    )
                    mnuLoadBackup.DropDownItems.Add(itm) |> ignore
        )
        let itmPrint = new ToolStripMenuItem("&Print", Assets.Prnt, (fun _ _ -> rep.PrintPreview()))
        let itmExit = new ToolStripMenuItem("E&xit", Assets.Exit, (fun _ _ -> this.Close()))
        mnuFile.DropDownItems.Add(mnuLoadBackup) |> ignore
        mnuFile.DropDownItems.Add(new ToolStripSeparator()) |> ignore
        mnuFile.DropDownItems.Add(itmPrint) |> ignore
        mnuFile.DropDownItems.Add(itmExit) |> ignore

        let mnuMode = new ToolStripMenuItem("&Mode")
        let itmEdit = new ToolStripMenuItem("Edit Mode (Build Repertoire)", Assets.Edt, fun _ _ -> setMode Edit)
        let itmRead = new ToolStripMenuItem("Read Mode (Practice)", Assets.Rd, fun _ _ -> setMode Read)
        let itmTrain = new ToolStripMenuItem("Train Mode (Test Yourself)", Assets.Trn, fun _ _ -> startTraining())
        mnuMode.DropDownItems.AddRange([| itmEdit :> ToolStripItem; itmRead; itmTrain |])

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
        let btnTrain = new ToolStripButton(Text = "Train", Image = Assets.Trn)
        btnTrain.Click.Add(fun _ -> startTraining())
        ts.Items.Add(btnWhite) |> ignore
        ts.Items.Add(btnBlack) |> ignore
        ts.Items.Add(new ToolStripSeparator()) |> ignore
        ts.Items.Add(btnSave) |> ignore
        ts.Items.Add(btnTrain) |> ignore
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
        setMode Read
        // --- Event Wiring ---
        rep.OnMovesSelected.Add(fun moves ->
            engine.Post StopSearch
            // 1. Reset Board and History UI
            let mutable tempBoard = Board.Start
            mh.Clear()
            // 2. Play through the sequence to rebuild history and board state
            for m in moves do
                let bdBefore = tempBoard
                mh.AddMove(bdBefore, m)
                tempBoard <- Board.MoveApply m tempBoard
            // 3. Set the final board position
            bd.SetBoard(tempBoard)
            refreshRep()
            // 4. Trigger analysis/Lichess for the new position
            let fen = FEN.FromBrd tempBoard
            lblPosition.Text <- sprintf "FEN: %s" (if fen.Length > 30 then fen.Substring(0, 27) + "..." else fen)
            ap.SetBoard(tempBoard)
            ap.Clear()
            async {
                let! data = LichessClient.fetchMastersStats fen
                match data with | Some d -> mr.UpdateData(d) | None -> ()
            } |> Async.Start
            engine.Post (SetPosition fen)
            engine.Post (StartSearch 10000)
        )
        bd.OnMoveMade.Add(fun (bdBefore, m) -> 
            engine.Post StopSearch
            if currentMode = Train then
                let isCorrect = currentTrainingCorrectMoves |> List.exists (fun am -> 
                    am.From = m.From && am.To = m.To && am.Prom = m.Prom)
                
                if isCorrect then
                    lblStatus.Text <- "Correct! Well done."
                    
                    let path = currentTrainingPositions.[trainingIndex]
                    let oldStat = Map.tryFind path trainingStats |> Option.defaultValue { Attempts = 0; Successes = 0; FailedLastTime = false }
                    let newStat = { oldStat with Attempts = oldStat.Attempts + 1; Successes = oldStat.Successes + 1; FailedLastTime = false }
                    trainingStats <- Map.add path newStat trainingStats
                    successCount <- successCount + 1
                    
                    trainingIndex <- trainingIndex + 1
                    loadTrainingPosition()
                else
                    attemptsLeft <- attemptsLeft - 1
                    lblStatus.Text <- sprintf "Incorrect move! Attempts left: %d" attemptsLeft
                    
                    if attemptsLeft <= 0 then
                        lblStatus.Text <- "Failed 3 times. Moving to next position."
                        
                        let path = currentTrainingPositions.[trainingIndex]
                        let oldStat = Map.tryFind path trainingStats |> Option.defaultValue { Attempts = 0; Successes = 0; FailedLastTime = false }
                        let newStat = { oldStat with Attempts = oldStat.Attempts + 1; FailedLastTime = true }
                        trainingStats <- Map.add path newStat trainingStats
                        
                        trainingIndex <- trainingIndex + 1
                        loadTrainingPosition()
                    else
                        let path = currentTrainingPositions.[trainingIndex]
                        let tempBoard = path |> List.fold (fun b mv -> Board.MoveApply mv b) Board.Start
                        bd.SetBoard(tempBoard)
                        
                        mh.Clear()
                        let mutable b = Board.Start
                        for mv in path do
                            mh.AddMove(b, mv)
                            b <- Board.MoveApply mv b
            else
                let oldHistory = mh.GetMoveList()
                let san = San.ToSan bdBefore m
                mh.AddMove(bdBefore, m)
                if currentMode = Edit then
                    currentRep <- Repertoire.update currentRep oldHistory m
                refreshRep()
                updateAllowedMoves(mh.GetMoveList())
                let currentBrd = bd.GetBoard()
                let fen = FEN.FromBrd currentBrd
                lblStatus.Text <- sprintf "Last move: %s" san
                lblPosition.Text <- sprintf "FEN: %s" (if fen.Length > 30 then fen.Substring(0, 27) + "..." else fen)
                ap.SetBoard(currentBrd)
                ap.Clear()
                async {
                    let! data = LichessClient.fetchMastersStats fen
                    match data with | Some d -> mr.UpdateData(d) | None -> ()
                } |> Async.Start
                engine.Post (SetPosition fen)
                engine.Post (StartSearch 10000)
        )
        mh.OnMoveSelected.Add(fun moves ->
            engine.Post StopSearch
            let mutable tempBoard = Board.Start
            mh.Clear()
            for m in moves do
                let bdBefore = tempBoard
                mh.AddMove(bdBefore, m)
                tempBoard <- Board.MoveApply m tempBoard
            bd.SetBoard(tempBoard)
            refreshRep()
            updateAllowedMoves(moves)
            let fen = FEN.FromBrd tempBoard
            lblPosition.Text <- sprintf "FEN: %s" (if fen.Length > 30 then fen.Substring(0, 27) + "..." else fen)
            ap.SetBoard(tempBoard)
            ap.Clear()
            async {
                let! data = LichessClient.fetchMastersStats fen
                match data with | Some d -> mr.UpdateData(d) | None -> ()
            } |> Async.Start
            engine.Post (SetPosition fen)
            engine.Post (StartSearch 10000)
        )
        rep.OnCommentUpdated.Add(fun (mvl, newComment) ->
            // Update the immutable state
            currentRep <- Repertoire.setComment currentRep mvl newComment
            // Save immediately
            Repertoire.save repfol currentRep
            // Refresh tree to keep the 'Tag' data in the UI in sync with the record
            refreshRep()
        )    

        currentRep <- Repertoire.load repfol WHITE
        bd.Orient(WHITE)
        refreshRep()
        lblStatus.Text <- "Studying White Repertoire"
    
    override this.OnFormClosing(e) =
        engine.Post Quit
        base.OnFormClosing(e)
        if currentMode = Edit then Repertoire.save repfol currentRep