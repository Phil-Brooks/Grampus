namespace Grampus

module Piece =
    let Colour (p: int) : int =
        p >>> 3
    let Parse(c : char) =
        match c with
        | 'P' -> WPAWN
        | 'N' -> WKNIGHT
        | 'B' -> WBISHOP
        | 'R' -> WROOK
        | 'Q' -> WQUEEN
        | 'K' -> WKING
        | 'p' -> BPAWN
        | 'n' -> BKNIGHT
        | 'b' -> BBISHOP
        | 'r' -> BROOK
        | 'q' -> BQUEEN
        | 'k' -> BKING
        | '.' -> EMPTY
        | _ -> failwith (c.ToString() + " is not a valid piece")
    let ToStr(piece : int) =
        match piece with
        | WPAWN -> "P"
        | WKNIGHT -> "N"
        | WBISHOP -> "B"
        | WROOK -> "R"
        | WQUEEN -> "Q"
        | WKING -> "K"
        | BPAWN -> "p"
        | BKNIGHT -> "n"
        | BBISHOP -> "b"
        | BROOK -> "r"
        | BQUEEN -> "q"
        | BKING -> "k"
        | EMPTY -> "."
        | _ -> failwith ("not a valid piece")
    let ToStr2(piece : int) =
        match piece with
        | WPAWN -> "wP"
        | WKNIGHT -> "wN"
        | WBISHOP -> "wB"
        | WROOK -> "wR"
        | WQUEEN -> "wQ"
        | WKING -> "wK"
        | BPAWN -> "bP"
        | BKNIGHT -> "bN"
        | BBISHOP -> "bB"
        | BROOK -> "bR"
        | BQUEEN -> "bQ"
        | BKING -> "bK"
        | EMPTY -> "."
        | _ -> failwith ("not a valid piece")
    let ToColour (piece : int) =
        if piece = EMPTY then 
            None 
        else
            Some (Colour piece)