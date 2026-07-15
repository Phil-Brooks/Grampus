namespace Grampus

[<AutoOpen>]
/// <namespacedoc>
///   <summary>This is the namespace containing all Grampus backend functionality.</summary>
/// </namespacedoc>
/// <summary>Holds all the main types used by Grampus.</summary>
module Types =
    
    let A1, B1, C1, D1, E1, F1, G1, H1 =
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
    let OUTOFBOUNDS : int = 64
    let SQUARES =
        [ A1; B1; C1; D1; E1; F1; G1; H1; A2; B2; C2; D2; E2; F2; G2; H2; A3; B3; 
          C3; D3; E3; F3; G3; H3; A4; B4; C4; D4; E4; F4; G4; H4; A5; B5; C5; D5; 
          E5; F5; G5; H5; A6; B6; C6; D6; E6; F6; G6; H6; A7; B7; C7; D7; E7; F7; 
          G7; H7; A8; B8; C8; D8; E8; F8; G8; H8 ]
    
    let Sq(f : int, r : int) : int = r * 8 + f
   
    
    
    /// <summary>Record type holding board details such as pieces on each square.</summary>
    type Brd =
        { 
          PieceAt : int[]
          WtKingPos : int
          BkKingPos : int
          PieceTypes : uint64 list
          WtPrBds : uint64
          BkPrBds : uint64
          PieceLocationsAll : uint64
          Checkers : uint64
          WhosTurn : int
          CastleRights : int
          EnPassant : int
          Fiftymove : int
          Fullmove : int }
        member bd.Item
            with get (sq : int) = bd.PieceAt.[sq]
        override bd.ToString() =
            let pctostr pc =
                match pc with
                | 1 -> "P"
                | 2 -> "N"
                | 3 -> "B"
                | 4 -> "R"
                | 5 -> "Q"
                | 6 -> "K"
                | 9 -> "p"
                | 10 -> "n"
                | 11 -> "b"
                | 12 -> "r"
                | 13 -> "q"
                | 14 -> "k"
                | 0 -> "."
                | _ -> failwith "invalid piece"
            
            let bdstr =
                bd.PieceAt
                |> Array.map (fun p -> p |> pctostr)
                |> String.concat ""
            
            let tomv =
                if bd.WhosTurn = 0 then " w"
                else " b"
            
            bdstr + tomv
    
    /// <summary>Record type holding details of an unencoded move such as pieces the target square.</summary>
    type pMove =
        { Mtype : int
          TargetSquare : int
          Piece : int option
          OriginFile : int option
          OriginRank : int option
          PromotedPiece : int option
          IsCheck : bool
          IsDoubleCheck : bool
          IsCheckMate : bool
          San : string }
        override x.ToString() = x.San

    type Fen =
        { Pieceat : int list
          Whosturn : int
          CastleWS : bool
          CastleWL : bool
          CastleBS : bool
          CastleBL : bool
          Enpassant : int
          Fiftymove : int
          Fullmove : int }
