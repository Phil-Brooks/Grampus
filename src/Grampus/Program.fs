namespace GrampusUI

open System
open System.Windows.Forms
open Form

module Main =
    [<EntryPoint; STAThread>]
    Application.SetHighDpiMode(HighDpiMode.SystemAware)|>ignore
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)

    let frm = new FrmMain()
    
    Application.Run(frm)
