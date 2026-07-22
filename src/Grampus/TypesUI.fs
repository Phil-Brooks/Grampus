namespace GrampusUI

open System.Drawing
open System.IO
open System.Text.Json

type AppConfig = {
    mutable EngineLocation : string
    mutable RepertoireFolder : string
    mutable PieceSet : string
    mutable ThemeColors : int list // Store as ARGB integers for easy JSON saving
}

module ConfigManager =
    let fileName = "settings.json"
    
    let defaultColors = [
        Color.Green.ToArgb()
        Color.PaleGreen.ToArgb()
        Color.YellowGreen.ToArgb()
        Color.Yellow.ToArgb()
    ]

    let defaultSettings = {
        EngineLocation = @"D:\Github\Grampus\engines\stockfish.exe"
        RepertoireFolder = @"D:\Rep\2026"
        PieceSet = "Merida"
        ThemeColors = defaultColors
    }

    let load () =
        if File.Exists(fileName) then
            try
                let json = File.ReadAllText(fileName)
                JsonSerializer.Deserialize<AppConfig>(json)
            with _ -> defaultSettings
        else defaultSettings

    let save (config: AppConfig) =
        let json = JsonSerializer.Serialize(config)
        File.WriteAllText(fileName, json)

[<AutoOpen>]
module TypesUI =
    let sqsz = 64
    let bdsz = 8 * sqsz
    let pnlsz = 10 * sqsz

    // options
    let mutable Settings = ConfigManager.load()
    let mutable engloc = Settings.EngineLocation
    let mutable repfol = Settings.RepertoireFolder
    let mutable uipcs = Settings.PieceSet
    let mutable uisqs = Settings.ThemeColors |> List.map Color.FromArgb
