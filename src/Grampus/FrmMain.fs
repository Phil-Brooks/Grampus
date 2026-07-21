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

    // --- State ---
    // Creating a small test repertoire for White
    let mutable currentRepertoire : Repertoire option = 
        Some { 
            Name = "White Opening Study"
            Side = WHITE
            Roots = [ 
                { Mv = { From=E2; To=E4; Pc=WPAWN; CapPc=EMPTY; Prom=EMPTY }; San = "e4"; Annotation = MainLine; Comment = "Best by test"; Replies = [
                    { Mv = { From=C7; To=C5; Pc=BPAWN; CapPc=EMPTY; Prom=EMPTY }; San = "c5"; Annotation = Opponent; Comment = "Sicilian Defense"; Replies = [
                         { Mv = { From=G1; To=F3; Pc=WKNIGHT; CapPc=EMPTY; Prom=EMPTY }; San = "Nf3"; Annotation = MainLine; Comment = "Open Sicilian"; Replies = [] }
                    ]}
                ]}
                { Mv = { From=D2; To=D4; Pc=WPAWN; CapPc=EMPTY; Prom=EMPTY }; San = "d4"; Annotation = Alternative; Comment = "Queen's Pawn Game"; Replies = [] }
            ] 
        }

    // --- Helper Logic ---
    let updateRepertoireUI() =
        match currentRepertoire with
        | Some r ->
            let history = mh.GetMoveList()
            match Repertoire.findCurrentBranch r.Roots history with
            | Some replies -> rep.UpdateMoves(replies)
            | None -> rep.Clear()
        | None -> rep.Clear()

    // 2. Setup the Engine logic
    let onEngineMsg = function
        | Info info -> ap.UpdateAnalysis(info)
        | BestMove m -> printfn "Engine suggests: %s" m
        | Ready -> printfn "Engine is ready"

    let engine = Engine.spawn engloc onEngineMsg

    let createToolbar() =
        let ts = new ToolStrip()
        let btnFlip = new ToolStripButton(Text = "Flip Board", DisplayStyle = ToolStripItemDisplayStyle.ImageAndText)
        btnFlip.Image <- Assets.Orient
        btnFlip.Padding <- Padding(5, 0, 5, 0) 
        btnFlip.Click.Add(fun _ -> bd.Orient())
        
        let btnNew = new ToolStripButton(Text = "New Game")
        btnNew.Click.Add(fun _ -> 
            ap.Clear()
            mh.Clear() // Clear the move history
            bd.SetBoard(Board.Start) // Reset the board
            updateRepertoireUI() // Reset repertoire to roots
            engine.Post (SetPosition FEN.StartStr)
            engine.Post (StartSearch 5000)
        )
        
        ts.Items.Add(btnFlip) |> ignore
        ts.Items.Add(new ToolStripSeparator()) |> ignore
        ts.Items.Add(btnNew) |> ignore
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
        updateRepertoireUI()

    override this.OnFormClosing(e) =
        engine.Post Quit
        base.OnFormClosing(e)