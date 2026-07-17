namespace Grampus

module PieceType =
    let [<Literal>] EMPTY = 0
    let [<Literal>] Pawn = 1
    let [<Literal>] Knight = 2
    let [<Literal>] Bishop = 3
    let [<Literal>] Rook = 4
    let [<Literal>] Queen = 5
    let [<Literal>] King = 6
    
    // Precomputed char table for speed
    let chars = [| '-'; 'p'; 'n'; 'b'; 'r'; 'q'; 'k' |]
    /// Converts a PieceType to its character representation ('p'..'k').
    let toChar (pt: int) : char =
        chars[pt]
    /// Converts a character to a PieceType.
    let fromChar (c: char) : int =
        match c with
        | 'p' | 'P' -> Pawn
        | 'n' | 'N' -> Knight
        | 'b' | 'B' -> Bishop
        | 'r' | 'R' -> Rook
        | 'q' | 'Q' -> Queen
        | 'k' | 'K' -> King
        | _ -> invalidArg "c" $"Invalid piece type char: {c}"
    let ForPlayer (player : int) (pt : int) : int =
        pt ||| (player <<< 3)
