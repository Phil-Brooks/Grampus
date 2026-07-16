namespace GrampusUI

open System.Drawing
open System.Windows.Forms

module Form =
    
    type FrmMain() as this =
        inherit Form(Text = "Grampus", WindowState = FormWindowState.Maximized, 
                     IsMdiContainer = true, Icon = Assets.Grampus)

        let bd = new PnlBoard(Dock = DockStyle.Fill)

        let createToolbar() =
            let ts = new ToolStrip()
    
            // 1. Flip Board Button
            let btnFlip = new ToolStripButton(Text = "Flip Board", DisplayStyle = ToolStripItemDisplayStyle.ImageAndText)
            btnFlip.Image <- Assets.Orient
            btnFlip.Padding <- Padding(5, 0, 5, 0) 
    
            btnFlip.Click.Add(fun _ -> 
                bd.Orient()
            )

            // 2. New Game Button (Example)
            let btnNew = new ToolStripButton(Text = "New Game")
            btnNew.Click.Add(fun _ -> 
                // logic to reset board
                ()
            )

            ts.Items.Add(btnFlip) |> ignore
            ts.Items.Add(new ToolStripSeparator()) |> ignore
            ts.Items.Add(btnNew) |> ignore
    
            ts
        let ts = createToolbar()

        let bgpnl =
            new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.Fixed3D)
        let lfpnl =
            new Panel(Dock = DockStyle.Left, BorderStyle = BorderStyle.Fixed3D, 
                      Width = 600)
        let rtpnl =
            new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.Fixed3D)
        let lftpnl =
            new Panel(Dock = DockStyle.Top, BorderStyle = BorderStyle.Fixed3D, 
                      Height = 600)
        let lfbpnl =
            new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.Fixed3D)
        let rttpnl =
            new Panel(Dock = DockStyle.Top, BorderStyle = BorderStyle.Fixed3D, 
                      Height = 350)
        let rtmpnl =
            new Panel(Dock = DockStyle.Top, BorderStyle = BorderStyle.Fixed3D, 
                      Height = 100)
        let rtbpnl =
            new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.Fixed3D)
        do 
            rtbpnl |> rtpnl.Controls.Add
            rtmpnl |> rtpnl.Controls.Add
            rttpnl |> rtpnl.Controls.Add
            rtpnl |> bgpnl.Controls.Add
            lfbpnl |> lfpnl.Controls.Add
            bd |> lftpnl.Controls.Add
            lftpnl |> lfpnl.Controls.Add
            lfpnl |> bgpnl.Controls.Add
            bgpnl |> this.Controls.Add
            ts |> this.Controls.Add
