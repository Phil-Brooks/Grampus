namespace Grampus

module PieceType =
    //let [<Literal>] EMPTY = 0
    //let [<Literal>] Pawn = 1
    //let [<Literal>] Knight = 2
    //let [<Literal>] Bishop = 3
    //let [<Literal>] Rook = 4
    //let [<Literal>] Queen = 5
    //let [<Literal>] King = 6
    
    //// Precomputed char table for speed
    //let chars = [| '-'; 'p'; 'n'; 'b'; 'r'; 'q'; 'k' |]

    ///// Converts a PieceType to its character representation ('p'..'k').
    //let toChar (pt: int) : char =
    //    chars[pt]

    ///// Converts a character to a PieceType.
    //let fromChar (c: char) : int =
    //    match c with
    //    | 'p' | 'P' -> Pawn
    //    | 'n' | 'N' -> Knight
    //    | 'b' | 'B' -> Bishop
    //    | 'r' | 'R' -> Rook
    //    | 'q' | 'Q' -> Queen
    //    | 'k' | 'K' -> King
    //    | _ -> invalidArg "c" $"Invalid piece type char: {c}"
    
    
    
    let Parse(c : char) =
        match c with
        | 'P' -> PieceType.Pawn
        | 'N' -> PieceType.Knight
        | 'B' -> PieceType.Bishop
        | 'R' -> PieceType.Rook
        | 'Q' -> PieceType.Queen
        | 'K' -> PieceType.King
        | 'p' -> PieceType.Pawn
        | 'n' -> PieceType.Knight
        | 'b' -> PieceType.Bishop
        | 'r' -> PieceType.Rook
        | 'q' -> PieceType.Queen
        | 'k' -> PieceType.King
        | _ -> failwith (c.ToString() + " is not a valid piece")
    
    let ForPlayer (player : int) (pt : PieceType) : Piece =
        (int (pt) ||| (player <<< 3)) |> Pc
