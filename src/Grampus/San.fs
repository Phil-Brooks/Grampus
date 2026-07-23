namespace Grampus

module San =
    // Using Unicode Figurines: 
    // White: ♔ (6), ♕ (5), ♖ (4), ♗ (3), ♘ (2)
    // Black: ♚ (14), ♛ (13), ♜ (12), ♝ (11), ♞ (10)
    let getPieceFigurine pc =
        match pc with
        | 2  | 10 -> "♘" // Knight (Often UIs use the outline version for both, or ♞ for black)
        | 3  | 11 -> "♗" // Bishop
        | 4  | 12 -> "♖" // Rook
        | 5  | 13 -> "♕" // Queen
        | 6  | 14 -> "♔" // King
        | _  -> ""       // Pawns have no figurine in SAN
    let getPieceChar pc =
        match pc % 8 with // using % 8 handles both white (1-6) and black (9-14)
        | 2 -> "N" | 3 -> "B" | 4 -> "R" | 5 -> "Q" | 6 -> "K"
        | _ -> "" // Pawns have no letter in SAN
    let ToSan (bd: Brd) (m: Mv) =
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
    /// Converts a standard SAN string (e.g. "Nf3", "Bxe5", "a8=Q") to Figurine notation.
    let ToFigurine (san: string) =
        if System.String.IsNullOrWhiteSpace(san) then ""
        else
            // 1. Handle the leading piece character
            let firstChar = san.[0]
            let mutable result = 
                match firstChar with
                | 'N' -> "♘" + san.Substring(1)
                | 'B' -> "♗" + san.Substring(1)
                | 'R' -> "♖" + san.Substring(1)
                | 'Q' -> "♕" + san.Substring(1)
                | 'K' -> "♔" + san.Substring(1)
                | _   -> san // Pawn moves (c4) or Castling (O-O)

            // 2. Handle Promotion (e.g., "a8=Q" -> "a8=♕")
            if result.Contains("=") then
                let index = result.IndexOf('=')
                if index + 1 < result.Length then
                    let promoChar = result.[index + 1]
                    let promoFig = 
                        match promoChar with
                        | 'N' -> "♘" | 'B' -> "♗" | 'R' -> "♖" | 'Q' -> "♕"
                        | _ -> promoChar.ToString()
                    result <- result.Substring(0, index + 1) + promoFig + result.Substring(index + 2)
            result