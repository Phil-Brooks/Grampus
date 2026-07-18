namespace Grampus

module PcType =
    // Precomputed char table for speed
    let chars = [| '-'; 'p'; 'n'; 'b'; 'r'; 'q'; 'k' |]
    /// Converts a PieceType to its character representation ('p'..'k').
    let toChar (pt: int) : char =
        chars[pt]
    /// Converts a character to a PieceType.
    let fromChar (c: char) : int =
        match c with
        | 'p' | 'P' -> PAWN
        | 'n' | 'N' -> KNIGHT
        | 'b' | 'B' -> BISHOP
        | 'r' | 'R' -> ROOK
        | 'q' | 'Q' -> QUEEN
        | 'k' | 'K' -> KING
        | _ -> invalidArg "c" $"Invalid piece type char: {c}"
    let Piece (colour : int) (pt : int) : int =
        pt ||| (colour <<< 3)
