namespace Grampus

module Notation =
    
    let fileChar f = char (int 'a' + f)
    let rankChar r = char (int '1' + r)
    let sqToAlg sq = 
        let f = sq % 8
        let r = sq / 8
        sprintf "%c%c" (fileChar f) (rankChar r)
    let getPieceChar pc =
        match pc % 8 with // using % 8 handles both white (1-6) and black (9-14)
        | 2 -> "N" | 3 -> "B" | 4 -> "R" | 5 -> "Q" | 6 -> "K"
        | _ -> "" // Pawns have no letter in SAN

    let ToSan (bd: Brd) (m: Move) =
        // 1. Check Castling
        if (m.Pc % 8 = 6) && abs(m.To - m.From) = 2 then
            if m.To % 8 = 6 then "O-O" else "O-O-O"
        else
            let piece = getPieceChar m.Pc
            let capture = if m.CapPc <> 0 then "x" else ""
            let target = sqToAlg m.To
            
            // Special pawn capture notation (e.g., exd5)
            if piece = "" && m.CapPc <> 0 then
                let fromFile = fileChar (m.From % 8)
                sprintf "%cx%s" fromFile target
            else
                let promo = if m.Prom <> 0 then "=" + (getPieceChar m.Prom) else ""
                sprintf "%s%s%s%s" piece capture target promo