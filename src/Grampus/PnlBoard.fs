namespace GrampusUI

open System.Drawing
open System.Windows.Forms
open Grampus

type PnlBoard() as bd =
    inherit Panel(Width = pnlsz, Height = pnlsz)
        
    // 1. Define the event. It will pass (BoardBeforeMove, MoveRecord, Evaluation)
    let moveMade = new Event<Brd * Move>()
        
    let mutable board = Grampus.Board.Start
    let mutable sqTo = -1
    let mutable cCur = Cursors.Default
    let mutable prompc = EMPTY
    let mutable isw = true
    
    let bdpnl = new Panel(Dock = DockStyle.Top, Height = pnlsz)
    let sqpnl = new Panel(Width = pnlsz, Height = pnlsz, Left = 29, Top = 13)
    let edges =
        let top = new Panel(BackgroundImage = Assets.Back, Width = bdsz + 8, Height = 8, Left = 24, Top = 6)
        let left = new Panel(BackgroundImage = Assets.Back, Width = 8, Height = bdsz + 14, Left = 24, Top = 8)
        let right = new Panel(BackgroundImage = Assets.Back, Width = 8, Height = bdsz + 14, Left = bdsz + 30, Top = 6)
        let bottom = new Panel(BackgroundImage = Assets.Back, Width = bdsz + 6, Height = 8, Left = 32, Top = bdsz + 14)
        [top; left; right; bottom]
    let sqs : PictureBox [] = Array.zeroCreate 64
    let flbls : Label [] = Array.zeroCreate 8
    let rlbls : Label [] = Array.zeroCreate 8
    let dlgprom() =
        let dlg = new Form(Text = "", Height = sqsz + 4, Width = sqsz * 4 + 4)
        dlg.FormBorderStyle <- FormBorderStyle.FixedToolWindow
        dlg.StartPosition <- FormStartPosition.Manual
        dlg.ControlBox <- false
        let mousePos = Cursor.Position
        dlg.Location <- new Point(mousePos.X, mousePos.Y)
        let sqs : PictureBox [] = Array.zeroCreate 4
        let bpcims = [Assets.Pieces.["bQ"]; Assets.Pieces.["bR"]; Assets.Pieces.["bN"]; Assets.Pieces.["bB"]]
        let wpcims = [Assets.Pieces.["wQ"]; Assets.Pieces.["wR"]; Assets.Pieces.["wN"]; Assets.Pieces.["wB"]]
        ///set pieces on squares
        let setsq i (sq : PictureBox) =
            let sq = new PictureBox(Height = sqsz, Width = sqsz, SizeMode = PictureBoxSizeMode.Zoom)
            sq.BackColor <- if i % 2 = 0 then Color.Green else Color.PaleGreen
            sq.Top <- 1
            sq.Left <- i * sqsz + 1
            sq.Image <- if board.WhosTurn = WHITE then wpcims.[i] else bpcims.[i]
            //events
            let pctps = if board.WhosTurn = WHITE then [ WQUEEN; WROOK; WKNIGHT; WBISHOP ] else [ BQUEEN; BROOK; BKNIGHT; BBISHOP ]
            sq.Click.Add(fun e -> 
                prompc <- pctps.[i]
                dlg.DialogResult <- DialogResult.OK
                dlg.Close())
            sqs.[i] <- sq
        do sqs |> Array.iteri setsq
           sqs |> Array.iter dlg.Controls.Add
        dlg.ShowDialog()
    ///set pieces on squares
    let setpcsmvs() =
        let setpc i c =
            sqs.[i].Image <- if c = "." then null else Assets.Pieces.[c]
        let setpcsmvs() =
            board.PieceAt
            |> Array.map Piece.ToStr2
            |> Array.iteri setpc
        if (bd.InvokeRequired) then 
            try 
                bd.Invoke(MethodInvoker(setpcsmvs)) |> ignore
            with _ -> ()
        else setpcsmvs()
    ///orient board
    let orient() =
        isw <- not isw
        let possq i (sq : PictureBox) =
            let r = RNK i
            let f = FL i
            if not isw then 
                sq.Top <- r * sqsz + 1
                sq.Left <- 7 * sqsz - f * sqsz + 1
            else 
                sq.Left <- f * sqsz + 1
                sq.Top <- 7 * sqsz - r * sqsz + 1
        sqs |> Array.iteri possq
        flbls
        |> Array.iteri (fun i l -> 
                if isw then l.Left <- i * sqsz + 30
                else l.Left <- 7 * sqsz - i * sqsz + 30)
        rlbls
        |> Array.iteri (fun i l -> 
                if isw then l.Top <- 7 * sqsz - i * sqsz + 16
                else l.Top <- i * sqsz + 16)
    ///highlight possible squares
    let highlightsqs sl =
        let odd i = ((FL i) + (RNK i)) % 2 = 0
        let cl i = if odd i then Color.Green else Color.PaleGreen
        let hl i = if odd i then Color.YellowGreen else Color.Yellow
        sqs |> Array.iteri (fun i sq -> sqs.[i].BackColor <- cl i)
        sl |> List.iter (fun s -> sqs.[s].BackColor <- hl s)
    /// Action for GiveFeedback
    let giveFeedback (e : GiveFeedbackEventArgs) =
        e.UseDefaultCursors <- false
        sqpnl.Cursor <- cCur
    /// Action for Drag Over
    let dragOver (e : DragEventArgs) = e.Effect <- DragDropEffects.Move
    /// Action for Drag Drop
    let dragDrop (p : PictureBox, e) =
        sqTo <- System.Convert.ToInt32(p.Tag)
        sqpnl.Cursor <- Cursors.Default
    /// Action for Mouse Down
    let mouseDown (p : PictureBox, e : MouseEventArgs) =
        if e.Button = MouseButtons.Left then 
            let sqFrom = System.Convert.ToInt32(p.Tag)
            let sqf : int = sqFrom
            let psmvs = sqf |> MoveGen.PossMoves board
            let pssqs = psmvs |> List.map (fun m -> m.To)
            pssqs |> highlightsqs
            let oimg = p.Image
            p.Image <- null
            p.Refresh()
            let c = board.PieceAt.[sqFrom] |> Piece.ToStr2
            cCur <- Assets.Cursors.[c]
            sqpnl.Cursor <- cCur
            if pssqs.Length > 0 && (p.DoDragDrop(oimg, DragDropEffects.Move) = DragDropEffects.Move) then 
                let mvl = psmvs |> List.filter (fun m -> m.To = sqTo)
                if mvl.Length = 1 then 
                    let oldBoard = board
                    board <- board |> Board.MoveApply mvl.Head
                    setpcsmvs()
                    moveMade.Trigger(oldBoard, mvl.Head)
                elif mvl.Length = 4 then 
                    prompc <- EMPTY // Reset before showing
                    let result = dlgprom()
                    if result = DialogResult.OK && prompc <> EMPTY then
                        // Use tryFind to safely locate the specific promotion move
                        let matchedMove = mvl |> List.tryFind (fun mv -> mv.Prom = prompc)
                        match matchedMove with
                        | Some mv ->
                            let oldBoard = board    
                            board <- board |> Board.MoveApply mv
                            setpcsmvs()
                            moveMade.Trigger(oldBoard, mv)
                        | None -> 
                            // This shouldn't happen, but if it does, snap back
                            p.Image <- oimg 
                    else
                        // User closed the dialog or cancelled
                        p.Image <- oimg
                else 
                    // No valid moves, snap back
                    p.Image <- oimg
            else p.Image <- oimg
            sqpnl.Cursor <- Cursors.Default
            [] |> highlightsqs
    /// creates file label
    let flbl i =
        let lbl = new Label()
        lbl.Text <- File.NAMES.[i]
        lbl.Font <- new Font("Arial", 12.0F, FontStyle.Bold, GraphicsUnit.Point, byte (0))
        lbl.ForeColor <- Color.Green
        lbl.Height <- 21
        lbl.Width <- sqsz
        lbl.TextAlign <- ContentAlignment.MiddleCenter
        lbl.Left <- i * sqsz + 30
        lbl.Top <- 8 * sqsz + 24
        flbls.[i] <- lbl
    /// creates rank label
    let rlbl i =
        let lbl = new Label()
        lbl.Text <- (i + 1).ToString()
        lbl.Font <- new Font("Arial", 12.0F, FontStyle.Bold, GraphicsUnit.Point, byte (0))
        lbl.ForeColor <- Color.Green
        lbl.Height <- sqsz
        lbl.Width <- 21
        lbl.TextAlign <- ContentAlignment.MiddleCenter
        lbl.Left <- 0
        lbl.Top <- 7 * sqsz - i * sqsz + 16
        rlbls.[i] <- lbl
    ///set board colours and position of squares
    let setsq i sqi =
        let r = RNK i
        let f = FL i
        let sq = new PictureBox(Height = sqsz, Width = sqsz, SizeMode = PictureBoxSizeMode.Zoom)
        sq.BackColor <- if (f + r) % 2 = 0 then Color.Green else Color.PaleGreen
        sq.Left <- f * sqsz + 1
        sq.Top <- 7 * sqsz - r * sqsz + 1
        sq.Tag <- i
        //events
        sq.MouseDown.Add(fun e -> mouseDown (sq, e))
        sq.DragDrop.Add(fun e -> dragDrop (sq, e))
        sq.AllowDrop <- true
        sq.DragOver.Add(dragOver)
        sq.GiveFeedback.Add(giveFeedback)
        sqs.[i] <- sq
    do 
        sqs |> Array.iteri setsq
        sqs |> Array.iter sqpnl.Controls.Add
        setpcsmvs()
        edges |> List.iter bdpnl.Controls.Add
        for i = 0 to 7 do flbl i
        flbls |> Array.iter bdpnl.Controls.Add
        for i = 0 to 7 do rlbl i
        rlbls |> Array.iter bdpnl.Controls.Add
        sqpnl |> bdpnl.Controls.Add
        bdpnl |> bd.Controls.Add
    ///Sets the Board to be displayed
    member bd.SetBoard(ibd : Brd) =
        board <- ibd
        setpcsmvs()
    ///Gets the Board to be displayed
    member bd.GetBoard() = board
    ///Orients the Board depending on whether White
    member bd.Orient() = orient()
    // Expose the event so the Form can see it
    [<CLIEvent>]
    member this.OnMoveMade = moveMade.Publish
