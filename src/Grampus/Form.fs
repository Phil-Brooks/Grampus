namespace GrampusUI

open System.IO
open System.Drawing
open System.Windows.Forms
open GrampusWinForms

module Form =
    let img nm =
        let thisExe = System.Reflection.Assembly.GetExecutingAssembly()
        let file = thisExe.GetManifestResourceStream("Grampus.Images." + nm)
        Image.FromStream(file)
    
    let ico nm =
        let thisExe = System.Reflection.Assembly.GetExecutingAssembly()
        let file = thisExe.GetManifestResourceStream("Grampus.Images." + nm)
        new Icon(file)
    
    type FrmMain() as this =
        inherit Form(Text = "Grampus", WindowState = FormWindowState.Maximized, 
                     IsMdiContainer = true, Icon = ico "grampus.ico")

        let bd = new PnlBoard(Dock = DockStyle.Fill)


        let bgpnl =
            new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.Fixed3D)
        let lfpnl =
            new Panel(Dock = DockStyle.Left, BorderStyle = BorderStyle.Fixed3D, 
                      Width = 400)
        let rtpnl =
            new Panel(Dock = DockStyle.Fill, BorderStyle = BorderStyle.Fixed3D)
        let lftpnl =
            new Panel(Dock = DockStyle.Top, BorderStyle = BorderStyle.Fixed3D, 
                      Height = 400)
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
            //ScincFuncs.Eco.Read("scid.eco")|>ignore
            rtbpnl |> rtpnl.Controls.Add
            rtmpnl |> rtpnl.Controls.Add
            rttpnl |> rtpnl.Controls.Add
            rtpnl |> bgpnl.Controls.Add
            lfbpnl |> lfpnl.Controls.Add
            bd |> lftpnl.Controls.Add
            lftpnl |> lfpnl.Controls.Add
            lfpnl |> bgpnl.Controls.Add
            bgpnl |> this.Controls.Add
