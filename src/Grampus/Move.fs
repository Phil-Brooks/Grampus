namespace Grampus

module Move =
    
    let CreateProm (pfrom : int) (pto : int) (piece : int) (captured : int) (promote : int) =
        {
            From = pfrom
            To = pto
            Pc = piece
            CapPc = captured
            Prom = promote
        }
    let Create (pfrom : int) (pto : int) (piece : int) (captured : int) =
        CreateProm pfrom pto piece captured EMPTY
    let IsCap(move : Mv) = move.CapPc <> EMPTY
    let IsProm(move : Mv) = move.Prom <> EMPTY
    let Colour(move : Mv) = move.Pc |> Piece.Colour
    let IsEP (bd: Brd) (move : Mv)  = 
        move|>IsCap && bd.PieceAt.[move.To] = EMPTY
    let IsCastle(move : Mv) =
        (move.Pc = WKING || move.Pc = BKING) && abs (move.From - move.To) = 2
    let IsDouble(move : Mv) =
        (move.Pc = WPAWN || move.Pc = BPAWN)
        && abs (move.From - move.To) = 16

