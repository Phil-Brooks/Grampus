namespace Grampus

open System.Diagnostics

type EngineRequest = 
    | SetPosition of string // FEN
    | StartSearch of int    // Milliseconds
    | StopSearch
    | Quit

module UciProtocol =
    let formatRequest = function
        | SetPosition fen -> sprintf "position fen %s" fen
        | StartSearch ms -> sprintf "go movetime %d" ms
        | StopSearch -> "stop"
        | Quit -> "quit"

    let parseLine (line: string) : EngineMsg option =
        if line.StartsWith "info" then 
            UciParser.parseInfo line |> Option.map Info
        elif line.StartsWith "bestmove" then
            let parts = line.Split(' ')
            if parts.Length >= 2 then Some (BestMove parts.[1]) else None
        elif line = "readyok" then
            Some Ready
        else 
            None

module Engine =
    let spawn (path: string) (onMsg: EngineMsg -> unit) =
        MailboxProcessor<EngineRequest>.Start(fun inbox ->
            let proc = new Process()
            proc.StartInfo <- ProcessStartInfo(path, 
                RedirectStandardInput = true, 
                RedirectStandardOutput = true, 
                UseShellExecute = false, 
                CreateNoWindow = true)
            proc.OutputDataReceived.Add(fun e ->
                if not (isNull e.Data) then
                    if e.Data.StartsWith "info" then 
                        UciParser.parseInfo e.Data |> Option.iter (Info >> onMsg)
                    elif e.Data.StartsWith "bestmove" then
                        let move = e.Data.Split(' ').[1]
                        onMsg (BestMove move)
                    elif e.Data = "readyok" then
                        onMsg Ready
            )
            proc.Start() |> ignore
            proc.BeginOutputReadLine()
            let send (s: string) = proc.StandardInput.WriteLine s
            send "uci"
            send "isready"
            let rec loop () = async {
                let! msg = inbox.Receive()
                match msg with
                | SetPosition fen -> 
                    send (sprintf "position fen %s" fen)
                    return! loop()
                | StartSearch ms ->
                    send (sprintf "go movetime %d" ms)
                    return! loop()
                | StopSearch ->
                    send "stop"
                    return! loop()
                | Quit ->
                    send "quit"
                    proc.Kill()
            }
            loop ()
        )