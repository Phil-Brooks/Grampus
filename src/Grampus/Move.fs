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
        CreateProm pfrom pto piece captured PieceType.EMPTY
    let MovingPieceType(move : int) = move >>> 12 &&& 0x7
    let MovingPlayer(move : Move) = move.Pc |> Piece.colour
    let IsCapture(move : Move) = move.CapPc <> Piece.EMPTY
    let IsPromotion(move : Move) = move.Prom <> PieceType.EMPTY
    let Promote(move : Move) =
        if move.Prom = PieceType.EMPTY then Piece.EMPTY
        else move.Prom |> PieceType.ForPlayer(move |> MovingPlayer)
    let IsEnPassant(move : Move) =
        move.Pc|>Piece.ToPieceType = PieceType.Pawn
        //TODO: should change this!
        && not (move |> IsCapture)
        && (move.From |> Square.ToFile) <> (move.To|> Square.ToFile)
    let IsCastle(move : Move) =
        move.Pc|>Piece.ToPieceType = PieceType.King && abs (move.From - move.To) = 2
    let IsPawnDoubleJump(move : Move) =
        move.Pc|>Piece.ToPieceType = PieceType.Pawn
        && abs (move.From - move.To) = 16

