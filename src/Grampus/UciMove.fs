namespace Grampus

module UciMove =
    let FromStr (bd: Brd) (uci: string) : Mv option =
        if uci.Length < 4 then None
        else
            let fromSq = Square.FromStr (uci.Substring(0, 2))
            let toSq = Square.FromStr (uci.Substring(2, 2))
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
                Some (Move.Create fromSq toSq pc epCapPc)
            else
                Some (Move.CreateProm fromSq toSq pc capPc promPc)
    let ToStr (m: Mv) =
        let getPieceChar pc =
            match pc with // using % 8 handles both white (1-6) and black (9-14)
            | 2 -> "n" | 3 -> "b" | 4 -> "r" | 5 -> "q" | 6 -> "k"
            | _ -> "" 
        let mfrom = m.From |> Square.ToStr
        let mto = m.To |> Square.ToStr
        let promo = if m.Prom <> 0 then getPieceChar m.Prom else ""
        sprintf "%s%s%s" mfrom mto promo
// "Whenever you see a Grampus.Types.Brd, use this function to display it"
[<assembly: System.Diagnostics.DebuggerDisplay("{Grampus.UciMove.ToStr(this)}", Target = typeof<Types.Mv>)>]
do ()