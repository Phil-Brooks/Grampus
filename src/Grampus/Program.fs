open GrampusUI.Form

open System
open System.Windows.Forms
open System.Drawing

[<EntryPoint; STAThread>]
let main argv =
    Application.SetHighDpiMode(HighDpiMode.SystemAware)|>ignore
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)

    let frm = new FrmMain()
    
    Application.Run(frm)
    0 // Return exit code