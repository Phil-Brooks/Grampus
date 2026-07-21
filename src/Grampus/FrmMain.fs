namespace GrampusUI

open System.Windows.Forms
open Grampus
open System
open System.Drawing

type FrmMain() as this =
    inherit Form(Text = "Grampus", WindowState = FormWindowState.Maximized, 
                    IsMdiContainer = true, Icon = Assets.Grampus)

    let bd = new PnlBoard(Dock = DockStyle.Fill)
    let mh = new MoveHistoryPanel(Dock = DockStyle.Fill)
    let ap = new EngineAnalysisPanel(Dock = DockStyle.Fill)
    let mr = new MasterDatabasePanel(Dock = DockStyle.Fill)

    // 2. Setup the Engine logic
    let onEngineMsg = function
        | Info info -> ap.UpdateAnalysis(info)
        | BestMove m -> printfn "Engine suggests: %s" m
        | Ready -> printfn "Engine is ready"

    // Change "to your actual engine executable path
    let engine = Engine.spawn @"D:\Github\Grampus\stockfish.exe" onEngineMsg

    let createToolbar() =
        let ts = new ToolStrip()
        let btnFlip = new ToolStripButton(Text = "Flip Board", DisplayStyle = ToolStripItemDisplayStyle.ImageAndText)
        btnFlip.Image <- Assets.Orient
        btnFlip.Padding <- Padding(5, 0, 5, 0) 
        btnFlip.Click.Add(fun _ -> bd.Orient())
        
        let btnNew = new ToolStripButton(Text = "New Game")
        btnNew.Click.Add(fun _ -> 
            // When starting a new game, tell engine to reset
            ap.Clear()
            engine.Post (SetPosition FEN.StartStr)
            engine.Post (StartSearch 5000)
        )
        
        ts.Items.Add(btnFlip) |> ignore
        ts.Items.Add(new ToolStripSeparator()) |> ignore
        ts.Items.Add(btnNew) |> ignore
        ts
    let ts = createToolbar()

    // 1. Create the 3 Main Columns
    let colHistory = new Panel(Dock = DockStyle.Left, Width = 200, BorderStyle = BorderStyle.FixedSingle)
    let colBoard   = new Panel(Dock = DockStyle.Left, Width = 600, BorderStyle = BorderStyle.FixedSingle)
    let colAnalysis = new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.FixedSingle)

    do 
        // --- 1. Assemble History (Far Left) ---
        mh.Dock <- DockStyle.Fill
        colHistory.Controls.Add(mh)

        // --- 2. Assemble Middle Column (Board + Master) ---
        // We set the board to the TOP with a fixed height
        bd.Dock <- DockStyle.Top
        bd.Height <- 600
        
        // We set the masterPanel to FILL the remaining space
        mr.Dock <- DockStyle.Fill
        
        // IMPORTANT: Add the FILL control first, then the TOP control.
        // WinForms will give the TOP control its height, and the FILL control the rest.
        colBoard.Controls.Add(mr) 
        colBoard.Controls.Add(bd)

        // --- 3. Assemble Right Column (Analysis) ---
        ap.Dock <- DockStyle.Top
        ap.Height <- 100
        
        // Create a filler for the bottom-right if you want one
        let filler = new Panel(Dock = DockStyle.Fill, BackColor = Color.FromArgb(240, 240, 240))
        
        colAnalysis.Controls.Add(filler)
        colAnalysis.Controls.Add(ap)

        // --- 4. Final Form Assembly (Order is Vital) ---
        // Add columns in the order you want them to claim space
        this.Controls.Add(colAnalysis) // Claims the remaining center/right
        this.Controls.Add(colBoard)    // Claims 600px from the current left
        this.Controls.Add(colHistory)  // Claims 200px from the far left
        this.Controls.Add(ts)           // Claims the top edge        
        
        
        // Wire board moves to the Engine
        bd.OnMoveMade.Add(fun (bdBefore, m) -> 
            mh.AddMove(bdBefore, m)
            
            // Get the state AFTER the move was made
            let currentBrd = bd.GetBoard() 
            let fen = FEN.FromBrd currentBrd
            // Fetch from Lichess asynchronously
            async {
                let! data = LichessClient.fetchMastersStats fen
                match data with
                | Some d -> mr.UpdateData(d)
                | None -> ()
            } |> Async.Start
            
            // Sync the Analysis Panel context so it can generate SAN
            ap.SetBoard(currentBrd)
            ap.Clear()

            // Tell engine to search
            engine.Post (SetPosition fen)
            engine.Post (StartSearch 10000) 
        )        

    // Ensure the engine process is killed when the app closes
    override this.OnFormClosing(e) =
        engine.Post Quit
        base.OnFormClosing(e)