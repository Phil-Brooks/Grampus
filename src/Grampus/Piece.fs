namespace Grampus

open System

module Piece =
    let [<Literal>] EMPTY = 0
    let [<Literal>] WPawn = 1
    let [<Literal>] WKnight = 2
    let [<Literal>] WBishop = 3
    let [<Literal>] WRook = 4
    let [<Literal>] WQueen = 5
    let [<Literal>] WKing = 6
    let [<Literal>] BPawn = 9
    let [<Literal>] BKnight = 10
    let [<Literal>] BBishop = 11
    let [<Literal>] BRook = 12
    let [<Literal>] BQueen = 13
    let [<Literal>] BKing = 14

    let colour (p: int) : int =
        int p >>> 3

    let kind (p: int) : int =
        int p &&& 0b111

    let toChar (p: int) : char =
        let c = PieceType.toChar (kind p)
        if colour p = 0 then Char.ToUpper c else c

    let fromChar (c: char) : int =
        let col = if Char.IsUpper c then 0 else 1
        let kind = PieceType.fromChar c
        (col <<< 3) ||| kind
    
    let Parse(c : char) =
        match c with
        | 'P' -> WPawn
        | 'N' -> WKnight
        | 'B' -> WBishop
        | 'R' -> WRook
        | 'Q' -> WQueen
        | 'K' -> WKing
        | 'p' -> BPawn
        | 'n' -> BKnight
        | 'b' -> BBishop
        | 'r' -> BRook
        | 'q' -> BQueen
        | 'k' -> BKing
        | '.' -> EMPTY
        | _ -> failwith (c.ToString() + " is not a valid piece")
    
    let ToStr(piece : int) =
        match piece with
        | WPawn -> "P"
        | WKnight -> "N"
        | WBishop -> "B"
        | WRook -> "R"
        | WQueen -> "Q"
        | WKing -> "K"
        | BPawn -> "p"
        | BKnight -> "n"
        | BBishop -> "b"
        | BRook -> "r"
        | BQueen -> "q"
        | BKing -> "k"
        | EMPTY -> "."
        | _ -> failwith ("not a valid piece")
    
    let ToPieceType(piece : int) = piece &&& 7 
    
    let PieceToPlayer (piece : int) =
        if piece = EMPTY then 
            None 
        else
            // Shift to get 0 for White, 1 for Black, then wrap in Some
            let playerValue = piece >>> 3
            Some (playerValue)