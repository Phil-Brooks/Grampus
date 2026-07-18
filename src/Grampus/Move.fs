namespace Grampus

module Move =
    let CreateProm (pfrom : int) (pto : int) (piece : int) (captured : int) (promoteType : int) =
        {
            From = pfrom
            To = pto
            Pc = piece
            CapPc = captured
            Prom = promoteType
        }
    let Create (pfrom : int) (pto : int) (piece : int)  (captured : int) =
        CreateProm pfrom pto piece captured PcType.EMPTY
    let IsCapture(move : Move) = move.CapPc <> Piece.EMPTY
    let IsPromotion(move : Move) = move.Prom <> PcType.EMPTY
    let Colour(move : Move) = move.Pc |> Piece.Colour
    let Promote(move : Move) =
        if move.Prom = PcType.EMPTY then Piece.EMPTY
        else move.Prom |> PcType.Piece(move |> Colour)
    let IsEnPassant(move : Move) =
        move.Pc|>Piece.ToPcType = PcType.Pawn
        //TODO: should change this!
        && not (move |> IsCapture)
        && (move.From |> Square.ToFile) <> (move.To|> Square.ToFile)
    let IsCastle(move : Move) =
        move.Pc|>Piece.ToPcType = PcType.King && abs (move.From - move.To) = 2
    let IsPawnDoubleJump(move : Move) =
        move.Pc|>Piece.ToPcType = PcType.Pawn
        && abs (move.From - move.To) = 16
    let PcType(move : int) = move >>> 12 &&& 0x7

