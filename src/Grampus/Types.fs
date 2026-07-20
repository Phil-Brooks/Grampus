namespace Grampus

[<AutoOpen>]
/// <namespacedoc>
///   <summary>This is the namespace containing all Grampus backend functionality.</summary>
/// </namespacedoc>
/// <summary>Holds all the main types used by Grampus.</summary>
module Types =
    // Colours
    let [<Literal>] WHITE = 0
    let [<Literal>] BLACK = 1
    // Pieces
    let [<Literal>] EMPTY = 0
    let [<Literal>] WPAWN = 1
    let [<Literal>] WKNIGHT = 2
    let [<Literal>] WBISHOP = 3
    let [<Literal>] WROOK = 4
    let [<Literal>] WQUEEN = 5
    let [<Literal>] WKING = 6
    let [<Literal>] BPAWN = 9
    let [<Literal>] BKNIGHT = 10
    let [<Literal>] BBISHOP = 11
    let [<Literal>] BROOK = 12
    let [<Literal>] BQUEEN = 13
    let [<Literal>] BKING = 14
    // Ranks
    let [<Literal>] R1 = 0
    let [<Literal>] R2 = 1
    let [<Literal>] R3 = 2
    let [<Literal>] R4 = 3
    let [<Literal>] R5 = 4
    let [<Literal>] R6 = 5
    let [<Literal>] R7 = 6
    let [<Literal>] R8 = 7
    // Files
    let [<Literal>] A = 0
    let [<Literal>] B = 1
    let [<Literal>] C = 2
    let [<Literal>] D = 3
    let [<Literal>] E = 4
    let [<Literal>] F = 5
    let [<Literal>] G = 6
    let [<Literal>] H = 7
    // Squares
    let [<Literal>] A1 = 0
    let [<Literal>] B1 = 1
    let [<Literal>] C1 = 2
    let [<Literal>] D1 = 3
    let [<Literal>] E1 = 4
    let [<Literal>] F1 = 5
    let [<Literal>] G1 = 6
    let [<Literal>] H1 = 7
    let [<Literal>] A2 = 8
    let [<Literal>] B2 = 9
    let [<Literal>] C2 = 10
    let [<Literal>] D2 = 11
    let [<Literal>] E2 = 12
    let [<Literal>] F2 = 13
    let [<Literal>] G2 = 14
    let [<Literal>] H2 = 15
    let [<Literal>] A3 = 16
    let [<Literal>] B3 = 17
    let [<Literal>] C3 = 18
    let [<Literal>] D3 = 19
    let [<Literal>] E3 = 20
    let [<Literal>] F3 = 21
    let [<Literal>] G3 = 22
    let [<Literal>] H3 = 23
    let [<Literal>] A4 = 24
    let [<Literal>] B4 = 25
    let [<Literal>] C4 = 26
    let [<Literal>] D4 = 27
    let [<Literal>] E4 = 28
    let [<Literal>] F4 = 29
    let [<Literal>] G4 = 30
    let [<Literal>] H4 = 31
    let [<Literal>] A5 = 32
    let [<Literal>] B5 = 33
    let [<Literal>] C5 = 34
    let [<Literal>] D5 = 35
    let [<Literal>] E5 = 36
    let [<Literal>] F5 = 37
    let [<Literal>] G5 = 38
    let [<Literal>] H5 = 39
    let [<Literal>] A6 = 40
    let [<Literal>] B6 = 41
    let [<Literal>] C6 = 42
    let [<Literal>] D6 = 43
    let [<Literal>] E6 = 44
    let [<Literal>] F6 = 45
    let [<Literal>] G6 = 46
    let [<Literal>] H6 = 47
    let [<Literal>] A7 = 48
    let [<Literal>] B7 = 49
    let [<Literal>] C7 = 50
    let [<Literal>] D7 = 51
    let [<Literal>] E7 = 52
    let [<Literal>] F7 = 53
    let [<Literal>] G7 = 54
    let [<Literal>] H7 = 55
    let [<Literal>] A8 = 56
    let [<Literal>] B8 = 57
    let [<Literal>] C8 = 58
    let [<Literal>] D8 = 59
    let [<Literal>] E8 = 60
    let [<Literal>] F8 = 61
    let [<Literal>] G8 = 62
    let [<Literal>] H8 = 63
    let [<Literal>] OUTOFBOUNDS = 64 
    let SQUARES =
        [ A1; B1; C1; D1; E1; F1; G1; H1; A2; B2; C2; D2; E2; F2; G2; H2; A3; B3; 
          C3; D3; E3; F3; G3; H3; A4; B4; C4; D4; E4; F4; G4; H4; A5; B5; C5; D5; 
          E5; F5; G5; H5; A6; B6; C6; D6; E6; F6; G6; H6; A7; B7; C7; D7; E7; F7; 
          G7; H7; A8; B8; C8; D8; E8; F8; G8; H8 ]
    // MvTypes
    let [<Literal>] SIMPLE = 0
    let [<Literal>] ENPASSANT = 1
    
    
    // functions
    let SQ(f : int, r : int) : int = r * 8 + f
    let RNK (sq : int) = sq / 8
    let FL (sq : int) = sq % 8
   
    type Move =
        {
            MvType : int
            From : int
            To : int
            Pc : int
            CapPc : int
            Prom : int
        }
    type Castle =
        {
            WK : bool
            WQ : bool
            BK : bool
            BQ : bool
        }
    
    /// <summary>Record type holding board details such as pieces on each square.</summary>
    type Brd =
        { 
          PieceAt : int[]
          WtKingPos : int
          BkKingPos : int
          WhosTurn : int
          CastleRts : Castle
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

    type PlayedMove = {
        San : string
        Eval : float option
    }

    type HistoryEntry = {
        MoveNumber : int
        White : PlayedMove
        Black : PlayedMove option
    }

    type Score = 
        | Centipawns of int
        | MateIn of int
        | Unknown

    type Analysis = {
        Depth : int
        Score : Score
        Nodes : int64
        Pv    : string list
        MultiPvIndex: int
    }

    type EngineMsg = 
        | Info of Analysis
        | BestMove of string
        | Ready