namespace Grampus

module Move =
    let Create (pfrom : int) (pto : int) (piece : int) 
        (captured : int) : int =
        pfrom ||| (pto <<< 6) ||| (piece <<< 12) 
         ||| (captured <<< 16)
    let CreateProm (pfrom : int) (pto : int) (piece : int) 
        (captured : int) (promoteType : int) : int =
        pfrom ||| (pto <<< 6) ||| (piece <<< 12) 
         ||| (captured <<< 16) ||| (promoteType <<< 20)
    let From(move : int) : int = move &&& 0x3F
    let To(move : int) : int = move >>> 6 &&& 0x3F
    let MovingPiece(move : int) = move >>> 12 &&& 0xF
    let PromoteType(move : int) = move >>> 20 &&& 0x7
    let CapturedPiece(move : int) = move >>> 16 &&& 0xF
    let Int2Move (mvi:int) =
        {
            From = mvi|>From
            To = mvi|>To
            Pc = mvi|>MovingPiece
            CapPc = mvi|>CapturedPiece
            Prom = mvi|>PromoteType
        }
    
    let MovingPieceType(move : int) = move >>> 12 &&& 0x7
    let MovingPlayer(move : int) = move >>> 15 &&& 0x1
    let IsCapture(move : int) = (move >>> 16 &&& 0xF) <> 0
    let IsPromotion(move : int) = (move >>> 20 &&& 0x7) <> 0
    let Promote(move : int) =
        if move
           |> PromoteType = PieceType.EMPTY then Piece.EMPTY
        else (move |> PromoteType) |> PieceType.ForPlayer(move |> MovingPlayer)
    let IsEnPassant(move : int) =
        move
        |> MovingPieceType = PieceType.Pawn
        && not (move |> IsCapture)
        && (move
            |> From
            |> Square.ToFile)
           <> (move
               |> To
               |> Square.ToFile)
    let IsCastle(move : int) =
        move
        |> MovingPieceType = PieceType.King
        && abs ((move |> From) - (move |> To)) = 2
    let IsPawnDoubleJump(move : int) =
        move
        |> MovingPieceType = PieceType.Pawn
        && abs ((move |> From) - (move |> To)) = 16
    let Move2Int (mv:Move) =
         mv.From ||| (mv.To <<< 6) ||| (mv.Pc <<< 12) 
         ||| (mv.CapPc <<< 16) ||| (mv.Prom <<< 20)

