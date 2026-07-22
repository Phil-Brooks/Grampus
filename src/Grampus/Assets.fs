namespace GrampusUI

open System.Drawing
open System.Windows.Forms
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
    let CreateCursorFromBitmap (bmp: Bitmap) (xHot: int) (yHot: int) =
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
    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
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
        let path = "Grampus.Images." + uipcs + "." + name + ".png"
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
    let Black = loadImage "black.png"
    let White = loadImage "white.png"
    let Sav = loadImage "sav.png"
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
            let cursor = CursorHelper.CreateCursorFromBitmap resizedBmp (cursorSize / 2) (cursorSize / 2)
            resizedBmp.Dispose()
            code, cursor)
        |> Map.ofList
