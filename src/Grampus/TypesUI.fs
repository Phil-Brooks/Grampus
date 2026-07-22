namespace GrampusUI

open System.Drawing

[<AutoOpen>]
module TypesUI =
    let sqsz = 64
    let bdsz = 8 * sqsz
    let pnlsz = 10 * sqsz

    // options
    let engloc = @"D:\Github\Grampus\engines\stockfish.exe"
    let repfol = @"D:\Rep\2026"
    let uipcs = "Merida"
    //let uipcs = "Cburnett"    
    //let uipcs = "Horsey"
    //let uisqs = [Color.Green;Color.PaleGreen;Color.YellowGreen;Color.Yellow]
    let uisqs = [Color.Red;Color.Pink;Color.PaleVioletRed;Color.HotPink]