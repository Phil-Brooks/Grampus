namespace GrampusUI

open System.Windows.Forms
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
    let mutable currentRepertoire = Repertoire.load WHITE
    // --- Helper Logic ---
    let updateRepertoireUI(history: Mv list) =
        match Repertoire.findCurrentBranch currentRepertoire.Roots history with
        | Some replies -> 
            rep.UpdateMoves(replies)
        | None -> 
            rep.Clear()
    let switchRepertoire (side: int) =
        Repertoire.save currentRepertoire
        currentRepertoire <- Repertoire.load side
        bd.Orient(side) 
        mh.Clear()
        bd.SetBoard(Board.Start)
        updateRepertoireUI([]) 
        lblStatus.Text <- sprintf "Studying %s Repertoire" (if side = WHITE then "White" else "Black")
    // 2. Setup the Engine logic
    let onEngineMsg = function
        | Info info -> ap.UpdateAnalysis(info)
        | BestMove m -> printfn "Engine suggests: %s" m
        | Ready -> printfn "Engine is ready"
    let engine = Engine.spawn engloc onEngineMsg
    // --- Menus ---
    let createMenu() =
        let ms = new MenuStrip()
        
        // File Menu
        let mnuFile = new ToolStripMenuItem("&File")
        let itmNew = new ToolStripMenuItem("&New Game", null, (fun _ _ -> switchRepertoire currentRepertoire.Side))
        let itmExit = new ToolStripMenuItem("E&xit", null, (fun _ _ -> this.Close()))
        mnuFile.DropDownItems.AddRange([| itmNew :> ToolStripItem; new ToolStripSeparator() :> ToolStripItem; itmExit |])

        // Study Menu
        let mnuStudy = new ToolStripMenuItem("&Study")
        let itmWhite = new ToolStripMenuItem("White Repertoire", null, (fun _ _ -> switchRepertoire WHITE))
        let itmBlack = new ToolStripMenuItem("Black Repertoire", null, (fun _ _ -> switchRepertoire BLACK))
        let itmSave = new ToolStripMenuItem("&Save Now", null, (fun _ _ -> Repertoire.save currentRepertoire))
        mnuStudy.DropDownItems.AddRange([| itmWhite :> ToolStripItem; itmBlack :> ToolStripItem; new ToolStripSeparator() :> ToolStripItem; itmSave |])

        ms.Items.Add(mnuFile) |> ignore
        ms.Items.Add(mnuStudy) |> ignore
        ms

    // --- Status Bar ---
    let createStatusBar() =
        let ss = new StatusStrip()
        ss.Items.AddRange([| lblStatus :> ToolStripItem; lblEngine; lblPosition |])
        ss
    
    let createToolbar() =
        let ts = new ToolStrip()
        let btnWhite = new ToolStripButton(Text = "Study White", CheckOnClick = true, Checked = true)
        let btnBlack = new ToolStripButton(Text = "Study Black", CheckOnClick = true)
        btnWhite.Click.Add(fun _ -> 
            btnBlack.Checked <- false
            switchRepertoire WHITE
        )
        btnBlack.Click.Add(fun _ -> 
            btnWhite.Checked <- false
            switchRepertoire BLACK
        )
        let btnSave = new ToolStripButton(Text = "Save Changes")
        //btnSave.Image <- Assets.Save // If you have a save icon
        btnSave.Click.Add(fun _ -> Repertoire.save currentRepertoire)

        ts.Items.Add(btnWhite) |> ignore
        ts.Items.Add(btnBlack) |> ignore
        ts.Items.Add(new ToolStripSeparator()) |> ignore
        ts.Items.Add(btnSave) |> ignore
        ts    

    // 1. Layout containers
    let colHistory = new Panel(Dock = DockStyle.Left, Width = 200, BorderStyle = BorderStyle.FixedSingle)
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

        rep.OnMoveSelected.Add(fun m ->
            bd.MakeMove(m) // This will trigger OnMoveMade automatically
        )

        bd.OnMoveMade.Add(fun (bdBefore, m) -> 
            let oldHistory = mh.GetMoveList()
            let san = San.ToSan bdBefore m

            currentRepertoire <- Repertoire.update currentRepertoire oldHistory m san
            mh.AddMove(bdBefore, m)
            
            let newHistory = oldHistory @ [ m ]
            updateRepertoireUI(newHistory) 
            
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

        currentRepertoire <- Repertoire.load WHITE
        bd.Orient(WHITE)
        updateRepertoireUI([])
        lblStatus.Text <- "Studying White Repertoire"

    override this.OnFormClosing(e) =
        engine.Post Quit
        base.OnFormClosing(e)
        Repertoire.save currentRepertoire