namespace GrampusUI

open System.Drawing
open System.Windows.Forms
open Grampus
open System.Runtime.InteropServices

module CursorHelper =
    
    [<StructLayout(LayoutKind.Sequential)>]
    type IconInfo =
        struct
            val mutable fIcon: bool
            val mutable xHotspot: int
            val mutable yHotspot: int
            val mutable hbmMask: nativeint
            val mutable hbmColor: nativeint
        end
    [<DllImport("user32.dll")>]
    extern bool GetIconInfo(nativeint hIcon, IconInfo& piconinfo)

    [<DllImport("user32.dll")>]
    extern nativeint CreateIconIndirect(IconInfo& piconinfo)

    [<DllImport("user32.dll")>]
    extern bool DestroyIcon(nativeint hIcon)

    /// Creates a Cursor from a Bitmap with a specific hotspot
    let createCursorFromBitmap (bmp: Bitmap) (xHot: int) (yHot: int) =
        let hIcon = bmp.GetHicon()
        let mutable tmp = IconInfo()
        GetIconInfo(hIcon, &tmp)|>ignore
    
        tmp.xHotspot <- xHot
        tmp.yHotspot <- yHot
        tmp.fIcon <- false // 'false' makes it a cursor instead of an icon
        let hCursor = CreateIconIndirect(&tmp)
    
        // Clean up the temporary icon handle created by GetHicon
        DestroyIcon(hIcon) |> ignore
    
        new Cursor(hCursor)

module Assets =
    let private assembly = System.Reflection.Assembly.GetExecutingAssembly()
    
    let loadIcon (name: string) =
        let path = "Grampus.Images." + name
        let stream = assembly.GetManifestResourceStream(path)
        if stream = null then 
            failwithf "Resource not found: %s. Ensure it is marked as 'Embedded Resource'." path
        new Icon(stream)

    let loadImage (name: string) =
        let path = "Grampus.Images." + name
        let stream = assembly.GetManifestResourceStream(path)
        if stream = null then 
            failwithf "Resource not found: %s. Ensure it is marked as 'Embedded Resource'." path
        new Bitmap(stream)
    
    let loadPiece (name: string) =
        let path = "Grampus.Images.Merida." + name + ".png"
        let stream = assembly.GetManifestResourceStream(path)
        if stream = null then 
            failwithf "Resource not found: %s. Ensure it is marked as 'Embedded Resource'." path
        new Bitmap(stream)

    let resizeBitmap (bmp: Bitmap) (newWidth: int) (newHeight: int) =
        let newBmp = new Bitmap(newWidth, newHeight)
        use g = Graphics.FromImage(newBmp)
        // High quality scaling settings
        g.InterpolationMode <- System.Drawing.Drawing2D.InterpolationMode.HighQualityBicubic
        g.SmoothingMode <- System.Drawing.Drawing2D.SmoothingMode.HighQuality
        g.PixelOffsetMode <- System.Drawing.Drawing2D.PixelOffsetMode.HighQuality
        g.DrawImage(bmp, 0, 0, newWidth, newHeight)
        newBmp
    
    let Back = loadImage "Back.jpg"
    let Orient= loadImage "orient.png"
    let Grampus = loadIcon "grampus.ico"
    
    let Pieces = 
        [ "wP"; "wN"; "wB"; "wR"; "wQ"; "wK"; 
          "bP"; "bN"; "bB"; "bR"; "bQ"; "bK" ]
        |> List.map (fun code -> code, loadPiece code)
        |> Map.ofList

    let Cursors = 
        [ "wP"; "wN"; "wB"; "wR"; "wQ"; "wK"; 
          "bP"; "bN"; "bB"; "bR"; "bQ"; "bK" ]
        |> List.map (fun code -> 
            let originalBmp = Pieces.[code]
            let cursorSize = 64 
            let resizedBmp = resizeBitmap originalBmp cursorSize cursorSize
            let cursor = CursorHelper.createCursorFromBitmap resizedBmp (cursorSize / 2) (cursorSize / 2)
            resizedBmp.Dispose()
            code, cursor)
        |> Map.ofList

[<AutoOpen>]
module PnlBoardLib =
    let sqsz = 64
    let bdsz = 8 * sqsz
    let pnlsz = 10 * sqsz
    type PnlBoard() as bd =
        inherit Panel(Width = pnlsz, Height = pnlsz)
        let mutable board = Grampus.Board.Start
        let mutable sqTo = -1
        let mutable cCur = Cursors.Default
        let mutable prompctp = PieceType.EMPTY
        let mutable isw = true
        let bdpnl = new Panel(Dock = DockStyle.Top, Height = pnlsz)
        let sqpnl = new Panel(Width = pnlsz, Height = pnlsz, Left = 29, Top = 13)
        
        let edges =
            [ new Panel(BackgroundImage = Assets.Back, Width = bdsz + 8, 
                        Height = 8, Left = 24, Top = 6)
              
              new Panel(BackgroundImage = Assets.Back, Width = 8, 
                        Height = bdsz + 14, Left = 24, Top = 8)
              
              new Panel(BackgroundImage = Assets.Back, Width = 8, 
                        Height = bdsz + 14, Left = bdsz + 30, Top = 6)
              
              new Panel(BackgroundImage = Assets.Back, Width = bdsz + 6, 
                        Height = 8, Left = 32, Top = bdsz + 14) ]
        
        let sqs : PictureBox [] = Array.zeroCreate 64
        let flbls : Label [] = Array.zeroCreate 8
        let rlbls : Label [] = Array.zeroCreate 8
        
        let dlgprom() =
            let dlg =
                new Form(Text = "", Height = sqsz + 4, Width = sqsz * 4 + 4, 
                         FormBorderStyle = FormBorderStyle.FixedToolWindow, 
                         StartPosition = FormStartPosition.Manual,
                         ControlBox = false)
            
            let mousePos = Cursor.Position
            dlg.Location <- new Point(mousePos.X, mousePos.Y)
            
            let sqs : PictureBox [] = Array.zeroCreate 4
            
            let bpcims =
                [ Assets.Pieces.["bQ"]
                  Assets.Pieces.["bR"]
                  Assets.Pieces.["bN"]
                  Assets.Pieces.["bB"] ]
            
            let wpcims =
                [ Assets.Pieces.["wQ"]
                  Assets.Pieces.["wR"]
                  Assets.Pieces.["wN"]
                  Assets.Pieces.["wB"] ]
            
            ///set pieces on squares
            let setsq i (sq : PictureBox) =
                let sq =
                    new PictureBox(Height = sqsz, Width = sqsz, 
                                   SizeMode = PictureBoxSizeMode.Zoom)
                sq.BackColor <- if i % 2 = 0 then Color.Green
                                else Color.PaleGreen
                sq.Top <- 1
                sq.Left <- i * sqsz + 1
                sq.Image <- if board.WhosTurn = 0 then wpcims.[i]
                            else bpcims.[i]
                //events
                let pctps =
                    [ PieceType.Queen; PieceType.Rook; PieceType.Knight; 
                      PieceType.Bishop ]
                sq.Click.Add(fun e -> 
                    prompctp <- pctps.[i]
                    dlg.DialogResult <- DialogResult.OK
                    dlg.Close())
                sqs.[i] <- sq
            
            do sqs |> Array.iteri setsq
               sqs |> Array.iter dlg.Controls.Add
            dlg.ShowDialog()
        
        //events
        let mvEvt = new Event<_>()
        
        ///set pieces on squares
        let setpcsmvs() =
            let setpcsmvs() =
                board.PieceAt
                |> Array.map Piece.ToStr2
                |> Array.iteri (fun i c -> 
                       sqs.[i].Image <- if c = "." then null
                                        else Assets.Pieces.[c])
            if (bd.InvokeRequired) then 
                try 
                    bd.Invoke(MethodInvoker(setpcsmvs)) |> ignore
                with _ -> ()
            else setpcsmvs()
        
        ///orient board
        let orient() =
            isw <- not isw
            let possq i (sq : PictureBox) =
                let r = i / 8
                let f = i % 8
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
            sqs
            |> Array.iteri (fun i sq -> 
                   sqs.[i].BackColor <- if (i % 8 + i / 8) % 2 = 0 then 
                                            Color.Green
                                        else Color.PaleGreen)
            sl
            |> List.iter (fun s -> 
                   sqs.[s].BackColor <- if (s % 8 + s / 8) % 2 = 0 then 
                                            Color.YellowGreen
                                        else Color.Yellow)
        
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
        
        let refreshPromoImages() =
            let pcre = if board.WhosTurn = 0 then "w" else "b"
            sqs.[0].Image <- Assets.Pieces[pcre + "Q"]
            sqs.[1].Image <- Assets.Pieces[pcre + "R"]
            sqs.[2].Image <- Assets.Pieces[pcre + "N"]
            sqs.[3].Image <- Assets.Pieces[pcre + "B"]
        
        /// Action for Mouse Down
        let mouseDown (p : PictureBox, e : MouseEventArgs) =
            if e.Button = MouseButtons.Left then 
                let sqFrom = System.Convert.ToInt32(p.Tag)
                let sqf : int = sqFrom
                let psmvs = sqf |> MoveGenerate.PossMoves board
                
                let pssqs =
                    psmvs
                    |> List.map (fun m -> m.To)
                pssqs |> highlightsqs
                let oimg = p.Image
                p.Image <- null
                p.Refresh()
                let c = board.PieceAt.[sqFrom] |> Piece.ToStr2
                cCur <- Assets.Cursors.[c]
                sqpnl.Cursor <- cCur
                if pssqs.Length > 0 
                   && (p.DoDragDrop(oimg, DragDropEffects.Move) = DragDropEffects.Move) then 
                    let mvl =
                        psmvs
                        |> List.filter (fun m -> m.To = sqTo)
                    if mvl.Length = 1 then 
                        board <- board |> Board.MoveApply mvl.Head
                        setpcsmvs()
                        mvl.Head |> mvEvt.Trigger

                    elif mvl.Length = 4 then 
                        prompctp <- PieceType.EMPTY // Reset before showing
                        refreshPromoImages()
                        let result = dlgprom()
                    
                        if result = DialogResult.OK && prompctp <> PieceType.EMPTY then
                            // Use tryFind to safely locate the specific promotion move
                            let matchedMove = mvl |> List.tryFind (fun mv -> mv.Prom = prompctp)
                        
                            match matchedMove with
                            | Some mv ->
                                board <- board |> Board.MoveApply mv
                                setpcsmvs()
                                mv |> mvEvt.Trigger
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
        let flbl i lbli =
            let lbl = new Label()
            lbl.Text <- File.NAMES.[i]
            lbl.Font <- new Font("Arial", 12.0F, FontStyle.Bold, 
                                 GraphicsUnit.Point, byte (0))
            lbl.ForeColor <- Color.Green
            lbl.Height <- 21
            lbl.Width <- sqsz
            lbl.TextAlign <- ContentAlignment.MiddleCenter
            lbl.Left <- i * sqsz + 30
            lbl.Top <- 8 * sqsz + 24
            flbls.[i] <- lbl
        
        /// creates rank label
        let rlbl i lbli =
            let lbl = new Label()
            lbl.Text <- (i + 1).ToString()
            lbl.Font <- new Font("Arial", 12.0F, FontStyle.Bold, 
                                 GraphicsUnit.Point, byte (0))
            lbl.ForeColor <- Color.Green
            lbl.Height <- sqsz
            lbl.Width <- 21
            lbl.TextAlign <- ContentAlignment.MiddleCenter
            lbl.Left <- 0
            lbl.Top <- 7 * sqsz - i * sqsz + 16
            rlbls.[i] <- lbl
        
        ///set board colours and position of squares
        let setsq i sqi =
            let r = i / 8
            let f = i % 8
            let sq =
                new PictureBox(Height = sqsz, Width = sqsz, 
                               SizeMode = PictureBoxSizeMode.Zoom)
            sq.BackColor <- if (f + r) % 2 = 0 then Color.Green
                            else Color.PaleGreen
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
            flbls |> Array.iteri flbl
            flbls |> Array.iter bdpnl.Controls.Add
            rlbls |> Array.iteri rlbl
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
        
        //publish
        ///Provides the Move made on the board
        member __.MvMade = mvEvt.Publish
