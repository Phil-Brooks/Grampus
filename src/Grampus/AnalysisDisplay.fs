namespace Grampus

module AnalysisDisplay =
    
    let formatScore (whosTurn: int) score =
        match score with
        | Centipawns cp -> 
            let normalized = if whosTurn = 1 then -cp else cp
            let v = float normalized / 100.0
            if v > 0.0 then sprintf "+%.2f" v else sprintf "%.2f" v
        | MateIn m -> 
            let normalized = if whosTurn = 1 then -m else m
            sprintf "#%d" normalized
        | Unknown -> "-"

    let formatNodes (n: int64) =
        if n > 1_000_000L then sprintf "%.1fM" (float n / 1_000_000.0)
        elif n > 1_000L then sprintf "%.1fk" (float n / 1_000.0)
        else n.ToString()

    let getSanPv (startBd: Brd) (moves: string list) =
        let mutable tempBd = startBd
        let mutable moveNum = startBd.Fullmove
        let mutable isWhite = (startBd.WhosTurn = 0)
        
        let sanMoves = 
            moves |> List.choose (fun uci ->
                match UciMove.fromString tempBd uci with
                | Some m ->
                    let san = San.ToSan tempBd m
                    let prefix = if isWhite then sprintf "%d. %s" moveNum san else san
                    tempBd <- Board.MoveApply m tempBd
                    if not isWhite then moveNum <- moveNum + 1
                    isWhite <- not isWhite
                    Some prefix
                | None -> None
            )
        String.concat " " sanMoves