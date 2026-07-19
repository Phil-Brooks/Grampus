namespace Grampus

module Move =
    
    let CreateAll (mvtype:int) (pfrom : int) (pto : int) (piece : int) (captured : int) (promote : int) =
        {
            MvType = mvtype
            From = pfrom
            To = pto
            Pc = piece
            CapPc = captured
            Prom = promote
        }
    let CreateEp (pfrom : int) (pto : int) (piece : int) (captured : int)  =
        CreateAll ENPASSANT pfrom pto piece captured EMPTY
    let CreateProm (pfrom : int) (pto : int) (piece : int) (captured : int) (promote : int) =
        CreateAll SIMPLE pfrom pto piece captured promote
    let Create (pfrom : int) (pto : int) (piece : int) (captured : int) =
        CreateProm pfrom pto piece captured EMPTY
    let IsCapture(move : Move) = move.CapPc <> EMPTY
    let IsPromotion(move : Move) = move.Prom <> EMPTY
    let Colour(move : Move) = move.Pc |> Piece.Colour
    let IsEnPassant(move : Move) = move.MvType = ENPASSANT
    let IsCastle(move : Move) =
        (move.Pc = WKING || move.Pc = BKING) && abs (move.From - move.To) = 2
    let IsPawnDoubleJump(move : Move) =
        (move.Pc = WPAWN || move.Pc = BPAWN)
        && abs (move.From - move.To) = 16

