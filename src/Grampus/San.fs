namespace Grampus

module San =
    let fileChar f = char (int 'a' + f)
    let rankChar r = char (int '1' + r)
    let sqToAlg sq = 
        let f = FL sq
        let r = RNK sq
        sprintf "%c%c" (fileChar f) (rankChar r)
    let getPieceChar pc =
        match pc % 8 with // using % 8 handles both white (1-6) and black (9-14)
        | 2 -> "N" | 3 -> "B" | 4 -> "R" | 5 -> "Q" | 6 -> "K"
        | _ -> "" // Pawns have no letter in SAN
    let ToSan (bd: Brd) (m: Move) =
        // 1. Check Castling
        if (m.Pc % 8 = KING) && abs(m.To - m.From) = 2 then
            if m.To|>FL = G then "O-O" else "O-O-O"
        else
            let pieceChar = getPieceChar m.Pc
            let target = sqToAlg m.To
            let promo = if m.Prom <> 0 then "=" + (getPieceChar m.Prom) else ""
        
            // Special pawn capture notation (e.g., exd5 or hxg1=Q)
            if pieceChar = "" && m.CapPc <> 0 then
                let fromFile = fileChar (FL m.From)
                sprintf "%cx%s%s" fromFile target promo
            else
                // Standard piece moves, pawn advances, and piece captures
                let capture = if m.CapPc <> 0 then "x" else ""
                sprintf "%s%s%s%s" pieceChar capture target promo