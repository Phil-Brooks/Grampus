namespace GrampusUI

open System.Windows.Forms
open Grampus
open System

type FrmMain() as this =
    inherit Form(Text = "Grampus", WindowState = FormWindowState.Maximized, 
                    IsMdiContainer = true, Icon = Assets.Grampus)

    let bd = new PnlBoard(Dock = DockStyle.Fill)
    let mh = new MoveHistoryPanel(Dock = DockStyle.Fill)
    
    // 1. Create the Analysis Panel
    let ap = new EngineAnalysisPanel(Dock = DockStyle.Fill)

    // 2. Setup the Engine logic
    let onEngineMsg = function
        | Info info -> ap.UpdateAnalysis(info)
        | BestMove m -> printfn "Engine suggests: %s" m
        | Ready -> printfn "Engine is ready"

    // Change "path/to/stockfish" to your actual engine executable path
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
    let bgpnl = new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.Fixed3D)
    let lfpnl = new Panel(Dock = DockStyle.Left, BorderStyle = BorderStyle.Fixed3D, Width = 600)
    let rtpnl = new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.Fixed3D)
    let lftpnl = new Panel(Dock = DockStyle.Top, BorderStyle = BorderStyle.Fixed3D, Height = 600)
    let lfbpnl = new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.Fixed3D)
    let rttpnl = new Panel(Dock = DockStyle.Top, BorderStyle = BorderStyle.Fixed3D, Height = 350)
    let rtmpnl = new Panel(Dock = DockStyle.Top, BorderStyle = BorderStyle.Fixed3D, Height = 100)
    let rtbpnl = new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.Fixed3D)

    do 
        // 3. Wire board moves to the Engine
        bd.OnMoveMade.Add(fun (bdBefore, m) -> 
            mh.AddMove(bdBefore, m)
            
            // Get the state AFTER the move was made
            let currentBrd = bd.GetBoard() 
            let fen = FEN.FromBrd currentBrd
            
            // Sync the Analysis Panel context so it can generate SAN
            ap.SetBoard(currentBrd)
            ap.Clear()

            // Tell engine to search
            engine.Post (SetPosition fen)
            engine.Post (StartSearch 10000) 
        )        
        ap |> rttpnl.Controls.Add
        rtbpnl |> rtpnl.Controls.Add
        rtmpnl |> rtpnl.Controls.Add
        rttpnl |> rtpnl.Controls.Add
        rtpnl |> bgpnl.Controls.Add
        mh |> lfbpnl.Controls.Add
        lfbpnl |> lfpnl.Controls.Add
        bd |> lftpnl.Controls.Add
        lftpnl |> lfpnl.Controls.Add
        lfpnl |> bgpnl.Controls.Add
        bgpnl |> this.Controls.Add
        ts |> this.Controls.Add

    // Ensure the engine process is killed when the app closes
    override this.OnFormClosing(e) =
        engine.Post Quit
        base.OnFormClosing(e)