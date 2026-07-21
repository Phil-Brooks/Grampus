namespace GrampusUI

open System.Windows.Forms
open Grampus

type FrmMain() as this =
    inherit Form(Text = "Grampus", WindowState = FormWindowState.Maximized, 
                    IsMdiContainer = true, Icon = Assets.Grampus)

    // --- Controls ---
    let bd = new PnlBoard(Dock = DockStyle.Top, Height = 600)
    let mh = new MoveHistoryPanel(Dock = DockStyle.Fill)
    let ap = new EngineAnalysisPanel(Dock = DockStyle.Top, Height = 100)
    let mr = new MasterDatabasePanel(Dock = DockStyle.Fill)
    let rep = new RepertoirePanel(Dock = DockStyle.Fill)

    let mutable currentRepertoire = Repertoire.load WHITE

    // --- Helper Logic ---
    let updateRepertoireUI() =
        let history = mh.GetMoveList()
        match Repertoire.findCurrentBranch currentRepertoire.Roots history with
        | Some replies -> rep.UpdateMoves(replies)
        | None -> rep.Clear()
    let switchRepertoire (side: int) =
        // 1. Save current progress before switching
        Repertoire.save currentRepertoire
        
        // 2. Load the new one
        currentRepertoire <- Repertoire.load side
        
        // 3. Orient the board automatically
        bd.Orient(side) 
        
        // 4. Reset the game and UI for the new study
        mh.Clear()
        bd.SetBoard(Board.Start)
        updateRepertoireUI() 

    // 2. Setup the Engine logic
    let onEngineMsg = function
        | Info info -> ap.UpdateAnalysis(info)
        | BestMove m -> printfn "Engine suggests: %s" m
        | Ready -> printfn "Engine is ready"

    let engine = Engine.spawn engloc onEngineMsg

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
    let ts = createToolbar()

    // 1. Layout containers
    let colHistory = new Panel(Dock = DockStyle.Left, Width = 200, BorderStyle = BorderStyle.FixedSingle)
    let colBoard   = new Panel(Dock = DockStyle.Left, Width = 600, BorderStyle = BorderStyle.FixedSingle)
    let colAnalysis = new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.FixedSingle)

    do 
        colHistory.Controls.Add(mh)
        colBoard.Controls.Add(mr) 
        colBoard.Controls.Add(bd)
        colAnalysis.Controls.Add(rep)
        colAnalysis.Controls.Add(ap)

        this.Controls.Add(colAnalysis) 
        this.Controls.Add(colBoard)    
        this.Controls.Add(colHistory)  
        this.Controls.Add(ts)           
        
        // --- Event Wiring ---

        // A. When a user double-clicks a move in the Repertoire Panel
        rep.OnMoveSelected.Add(fun m ->
            bd.MakeMove(m) // This will trigger OnMoveMade automatically
        )

        // B. When a move is made (either by dragging or by selecting from book)
        bd.OnMoveMade.Add(fun (bdBefore, m) -> 
            mh.AddMove(bdBefore, m)
            updateRepertoireUI() // Update the suggested moves
            
            let currentBrd = bd.GetBoard() 
            let fen = FEN.FromBrd currentBrd
            
            async {
                let! data = LichessClient.fetchMastersStats fen
                match data with
                | Some d -> mr.UpdateData(d)
                | None -> ()
            } |> Async.Start
            
            ap.SetBoard(currentBrd)
            ap.Clear()
            engine.Post (SetPosition fen)
            engine.Post (StartSearch 10000) 
        )

        // C. Initial Display
        currentRepertoire <- Repertoire.load WHITE
        bd.Orient(WHITE)
        updateRepertoireUI()

    override this.OnFormClosing(e) =
        engine.Post Quit
        base.OnFormClosing(e)
        Repertoire.save currentRepertoire