namespace Grampus

[<AutoOpen>]
/// <namespacedoc>
///   <summary>This is the namespace containing all Grampus backend functionality.</summary>
/// </namespacedoc>
/// <summary>Holds all the main types used by Grampus.</summary>
module Types =
    /// <summary>Unsigned integer encoded to hold Move information.</summary>
    type Move = uint32

    /// <summary>Enum holding each type of piece e.g. 1 for Pawn.</summary>
    type PieceType =
        | EMPTY = 0
        | Pawn = 1
        | Knight = 2
        | Bishop = 3
        | Rook = 4
        | Queen = 5
        | King = 6
    
    /// <summary>Enum holding each type of piece for each colour e.g. 1 for WPawn.</summary>
    type Piece =
        | WPawn = 1
        | WKnight = 2
        | WBishop = 3
        | WRook = 4
        | WQueen = 5
        | WKing = 6
        | BPawn = 9
        | BKnight = 10
        | BBishop = 11
        | BRook = 12
        | BQueen = 13
        | BKing = 14
        | EMPTY = 0
    
    /// <summary>Short encoded to hold Rank.</summary>
    type Rank = int
    
    let Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8 : Rank * Rank * Rank * Rank * Rank * Rank * Rank * Rank =
        0, 1, 2, 3, 4, 5, 6, 7
    let RANKS = [ Rank1; Rank2; Rank3; Rank4; Rank5; Rank6; Rank7; Rank8 ]
    let RANK_NAMES = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" ]
    let RANK_EMPTY : Rank = 8
    
    /// <summary>Short encoded to hold Square.</summary>
    type Square = int
    
    let A1, B1, C1, D1, E1, F1, G1, H1 : Square * Square * Square * Square * Square * Square * Square * Square =
        0, 1, 2, 3, 4, 5, 6, 7
    let A2, B2, C2, D2, E2, F2, G2, H2 =
        A1 + 8, B1 + 8, C1 + 8, D1 + 8, E1 + 8, F1 + 8, G1 + 8, H1 + 8
    let A3, B3, C3, D3, E3, F3, G3, H3 =
        A2 + 8, B2 + 8, C2 + 8, D2 + 8, E2 + 8, F2 + 8, G2 + 8, H2 + 8
    let A4, B4, C4, D4, E4, F4, G4, H4 =
        A3 + 8, B3 + 8, C3 + 8, D3 + 8, E3 + 8, F3 + 8, G3 + 8, H3 + 8
    let A5, B5, C5, D5, E5, F5, G5, H5 =
        A4 + 8, B4 + 8, C4 + 8, D4 + 8, E4 + 8, F4 + 8, G4 + 8, H4 + 8
    let A6, B6, C6, D6, E6, F6, G6, H6 =
        A5 + 8, B5 + 8, C5 + 8, D5 + 8, E5 + 8, F5 + 8, G5 + 8, H5 + 8

    let A7, B7, C7, D7, E7, F7, G7, H7 =
        A6 + 8, B6 + 8, C6 + 8, D6 + 8, E6 + 8, F6 + 8, G6 + 8, H6 + 8
    let A8, B8, C8, D8, E8, F8, G8, H8 =
        A7 + 8, B7 + 8, C7 + 8, D7 + 8, E7 + 8, F7 + 8, G7 + 8, H7 + 8
    let OUTOFBOUNDS : Square = 64
    let SQUARES =
        [ A1; B1; C1; D1; E1; F1; G1; H1; A2; B2; C2; D2; E2; F2; G2; H2; A3; B3; 
          C3; D3; E3; F3; G3; H3; A4; B4; C4; D4; E4; F4; G4; H4; A5; B5; C5; D5; 
          E5; F5; G5; H5; A6; B6; C6; D6; E6; F6; G6; H6; A7; B7; C7; D7; E7; F7; 
          G7; H7; A8; B8; C8; D8; E8; F8; G8; H8 ]
    
    let Sq(f : int, r : Rank) : Square = r * 8 + f
    
    [<System.Flags>]
    /// <summary>Enum holding each type of castling e.g. 1 for WhiteShort.</summary>
    type CstlFlgs =
        | EMPTY = 0
        | WhiteShort = 1
        | WhiteLong = 2
        | BlackShort = 4
        | BlackLong = 8
        | All = 15
    
    [<System.Flags>]
    /// <summary>Enum holding bitboards for squares/ranks, e.g. 1UL for A1.</summary>
    type Bitboard =
        | A1 = 1UL
        | B1 = 2UL
        | C1 = 4UL
        | D1 = 8UL
        | E1 = 16UL
        | F1 = 32UL
        | G1 = 64UL
        | H1 = 128UL
        | A2 = 256UL
        | B2 = 512UL
        | C2 = 1024UL
        | D2 = 2048UL
        | E2 = 4096UL
        | F2 = 8192UL
        | G2 = 16384UL
        | H2 = 32768UL
        | A3 = 65536UL
        | B3 = 131072UL
        | C3 = 262144UL
        | D3 = 524288UL
        | E3 = 1048576UL
        | F3 = 2097152UL
        | G3 = 4194304UL
        | H3 = 8388608UL
        | A4 = 16777216UL
        | B4 = 33554432UL
        | C4 = 67108864UL
        | D4 = 134217728UL
        | E4 = 268435456UL
        | F4 = 536870912UL
        | G4 = 1073741824UL
        | H4 = 2147483648UL
        | A5 = 4294967296UL
        | B5 = 8589934592UL
        | C5 = 17179869184UL
        | D5 = 34359738368UL
        | E5 = 68719476736UL
        | F5 = 137438953472UL
        | G5 = 274877906944UL
        | H5 = 549755813888UL
        | A6 = 1099511627776UL
        | B6 = 2199023255552UL
        | C6 = 4398046511104UL
        | D6 = 8796093022208UL
        | E6 = 17592186044416UL
        | F6 = 35184372088832UL
        | G6 = 70368744177664UL
        | H6 = 140737488355328UL
        | A7 = 281474976710656UL
        | B7 = 562949953421312UL
        | C7 = 1125899906842624UL
        | D7 = 2251799813685248UL
        | E7 = 4503599627370496UL
        | F7 = 9007199254740992UL
        | G7 = 18014398509481984UL
        | H7 = 36028797018963968UL
        | A8 = 72057594037927936UL
        | B8 = 144115188075855872UL
        | C8 = 288230376151711744UL
        | D8 = 576460752303423488UL
        | E8 = 1152921504606846976UL
        | F8 = 2305843009213693952UL
        | G8 = 4611686018427387904UL
        | H8 = 9223372036854775808UL
        | Rank8 = 18374686479671623680UL
        | Rank7 = 71776119061217280UL
        | Rank6 = 280375465082880UL
        | Rank5 = 1095216660480UL
        | Rank4 = 4278190080UL
        | Rank3 = 16711680UL
        | Rank2 = 65280UL
        | Rank1 = 255UL
        | FileA = 72340172838076673UL
        | FileB = 144680345676153346UL
        | FileC = 289360691352306692UL
        | FileD = 578721382704613384UL
        | FileE = 1157442765409226768UL
        | FileF = 2314885530818453536UL
        | FileG = 4629771061636907072UL
        | FileH = 9259542123273814144UL
        | Empty = 0UL
        | Full = 18446744073709551615UL
    
    /// <summary>Option holding each type of move e.g. Capture.</summary>
    type MoveType =
        | Simple
        | Capture
        | CastleKingSide
        | CastleQueenSide
    
    /// <summary>Record type holding board details such as pieces on each square.</summary>
    type Brd =
        { 
          PieceAt : Piece[]
          WtKingPos : Square
          BkKingPos : Square
          PieceTypes : Bitboard list
          WtPrBds : Bitboard
          BkPrBds : Bitboard
          PieceLocationsAll : Bitboard
          Checkers : Bitboard
          WhosTurn : int
          CastleRights : CstlFlgs
          EnPassant : Square
          Fiftymove : int
          Fullmove : int }
        member bd.Item
            with get (sq : Square) = bd.PieceAt.[int (sq)]
        override bd.ToString() =
            let pctostr pc =
                match pc with
                | Piece.WPawn -> "P"
                | Piece.WKnight -> "N"
                | Piece.WBishop -> "B"
                | Piece.WRook -> "R"
                | Piece.WQueen -> "Q"
                | Piece.WKing -> "K"
                | Piece.BPawn -> "p"
                | Piece.BKnight -> "n"
                | Piece.BBishop -> "b"
                | Piece.BRook -> "r"
                | Piece.BQueen -> "q"
                | Piece.BKing -> "k"
                | Piece.EMPTY -> "."
                | _ -> failwith "invalid piece"
            
            let bdstr =
                bd.PieceAt
                |> Array.map (fun p -> p |> pctostr)
                |> String.concat ""
            
            let tomv =
                if bd.WhosTurn = 0 then " w"
                else " b"
            
            bdstr + tomv
    
    let BrdEMP =
        { PieceAt = Array.create 64 Piece.EMPTY
          WtKingPos = OUTOFBOUNDS
          BkKingPos = OUTOFBOUNDS
          PieceTypes = Array.create 7 Bitboard.Empty |> List.ofArray
          WtPrBds = Bitboard.Empty
          BkPrBds = Bitboard.Empty
          PieceLocationsAll = Bitboard.Empty
          Checkers = Bitboard.Empty
          WhosTurn = 0
          CastleRights = CstlFlgs.EMPTY
          EnPassant = OUTOFBOUNDS
          Fiftymove = 0
          Fullmove = 0 }
    
    /// <summary>Record type holding details of an unencoded move such as pieces the target square.</summary>
    type pMove =
        { Mtype : MoveType
          TargetSquare : Square
          Piece : PieceType option
          OriginFile : int option
          OriginRank : Rank option
          PromotedPiece : PieceType option
          IsCheck : bool
          IsDoubleCheck : bool
          IsCheckMate : bool
          San : string }
        override x.ToString() = x.San

    type Fen =
        { Pieceat : Piece list
          Whosturn : int
          CastleWS : bool
          CastleWL : bool
          CastleBS : bool
          CastleBL : bool
          Enpassant : Square
          Fiftymove : int
          Fullmove : int }
