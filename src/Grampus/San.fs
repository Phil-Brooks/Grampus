namespace Grampus

module San =
    let getPieceChar pc =
        match pc % 8 with // using % 8 handles both white (1-6) and black (9-14)
        | 2 -> "N" | 3 -> "B" | 4 -> "R" | 5 -> "Q" | 6 -> "K"
        | _ -> "" // Pawns have no letter in SAN
    let ToSan (bd: Brd) (m: Move) =
        // 1. Check Castling
        if (m.Pc = WKING || m.Pc = BKING) && abs(m.To - m.From) = 2 then
            if m.To|>FL = G then "O-O" else "O-O-O"
        else
            let pieceChar = getPieceChar m.Pc
            let target = m.To |> Square.ToStr
            let promo = if m.Prom <> 0 then "=" + (getPieceChar m.Prom) else ""
        
            // Special pawn capture notation (e.g., exd5 or hxg1=Q)
            if pieceChar = "" && m.CapPc <> 0 then
                let fromFile = File.ToStr (FL m.From)
                sprintf "%sx%s%s" fromFile target promo
            else
                // Standard piece moves, pawn advances, and piece captures
                let capture = if m.CapPc <> 0 then "x" else ""
                sprintf "%s%s%s%s" pieceChar capture target promo