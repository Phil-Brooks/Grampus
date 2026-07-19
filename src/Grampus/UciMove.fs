namespace Grampus

module UciMove =
    let fromString (bd: Brd) (uci: string) : Move option =
        if uci.Length < 4 then None
        else
            let fromSq = Square.Parse (uci.Substring(0, 2))
            let toSq = Square.Parse (uci.Substring(2, 2))
            let pc = bd.PieceAt.[fromSq]
            let capPc = bd.PieceAt.[toSq]
            
            // Handle Promotion (e.g. "a7a8q")
            let promPc = 
                if uci.Length = 5 then
                    match uci.[4] with
                    | 'q' -> if Piece.IsWhite pc then WQUEEN else BQUEEN
                    | 'r' -> if Piece.IsWhite pc then WROOK else BROOK
                    | 'b' -> if Piece.IsWhite pc then WBISHOP else BBISHOP
                    | 'n' -> if Piece.IsWhite pc then WKNIGHT else BKNIGHT
                    | _ -> EMPTY
                else EMPTY

            // Handle En Passant
            if (pc = WPAWN || pc = BPAWN) && toSq = bd.EnPassant && capPc = EMPTY then
                let epCapPc = if pc = WPAWN then BPAWN else WPAWN
                Some (Move.CreateEp fromSq toSq pc epCapPc)
            else
                Some (Move.CreateAll SIMPLE fromSq toSq pc capPc promPc)