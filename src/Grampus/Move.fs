namespace Grampus

module Move =
    let Create (pfrom : int) (pto : int) (piece : Piece) 
        (captured : Piece) : int =
        pfrom ||| (pto <<< 6) ||| (int (piece) <<< 12) 
         ||| (int (captured) <<< 16)
    let CreateProm (pfrom : int) (pto : int) (piece : Piece) 
        (captured : Piece) (promoteType : PieceType) : int =
        pfrom ||| (pto <<< 6) ||| (int (piece) <<< 12) 
         ||| (int (captured) <<< 16) ||| (int (promoteType) <<< 20)
    let From(move : int) : int = int (move) &&& 0x3F
    let To(move : int) : int = int (move) >>> 6 &&& 0x3F
    let MovingPiece(move : int) = (int (move) >>> 12 &&& 0xF) |> Pc
    
    let MovingPieceType(move : int) = (int (move) >>> 12 &&& 0x7) |> PcTp
    let MovingPlayer(move : int) = (int (move) >>> 15 &&& 0x1) 
    let IsCapture(move : int) = (int (move) >>> 16 &&& 0xF) <> 0
    let CapturedPiece(move : int) = (int (move) >>> 16 &&& 0xF) |> Pc
    let IsPromotion(move : int) = (int (move) >>> 20 &&& 0x7) <> 0
    let PromoteType(move : int) = (int (move) >>> 20 &&& 0x7) |> PcTp
    
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
        && abs (int (move |> From) - int (move |> To)) = 2
    let IsPawnDoubleJump(move : int) =
        move
        |> MovingPieceType = PieceType.Pawn
        && abs (int (move |> From) - int (move |> To)) = 16
