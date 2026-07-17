namespace Grampus

module Board =
    let EMPTY =
        { PieceAt = Array.create 64 0
          WtKingPos = OUTOFBOUNDS
          BkKingPos = OUTOFBOUNDS
          WhosTurn = 0
          CastleRights = 0
          EnPassant = OUTOFBOUNDS
          Fiftymove = 0
          Fullmove = 0 }
    
    let private PieceMove (mfrom : int) mto (bd : Brd) =
        let piece = bd.PieceAt.[mfrom]
        let player = (piece |> Piece.PieceToPlayer).Value
        let pieceType = piece |> Piece.ToPieceType
        
        let pieceat =
            bd.PieceAt
            |> Array.mapi (fun i p -> 
                   if i = int (mto) then piece
                   elif i = int (mfrom) then Piece.EMPTY
                   else p)
        
        let wtkingpos =
            if pieceType = PieceType.King && player = 0 then mto
            else bd.WtKingPos
        
        let bkkingpos =
            if pieceType = PieceType.King && player = 1 then mto
            else bd.BkKingPos
        
        { bd with PieceAt = pieceat
                  WtKingPos = wtkingpos
                  BkKingPos = bkkingpos }
    
    let PieceAdd pos (piece : int) (bd : Brd) =
        let player = (piece |> Piece.PieceToPlayer).Value
        let pieceType = piece |> Piece.ToPieceType
        
        let pieceat =
            bd.PieceAt
            |> Array.mapi (fun i p -> 
                   if i = int (pos) then piece
                   else p)
        
        let wtkingpos =
            if pieceType = PieceType.King && player = 0 then pos
            else bd.WtKingPos
        
        let bkkingpos =
            if pieceType = PieceType.King && player = 1 then pos
            else bd.BkKingPos
        
        { bd with PieceAt = pieceat
                  WtKingPos = wtkingpos
                  BkKingPos = bkkingpos }
    
    let PieceRemove (pos : int) (bd : Brd) =
        let piece = bd.PieceAt.[pos]
        let player = (piece |> Piece.PieceToPlayer).Value
        let pieceType = piece |> Piece.ToPieceType
        
        let pieceat =
            bd.PieceAt
            |> Array.mapi (fun i p -> 
                   if i = pos then Piece.EMPTY
                   else p)
        
        { bd with PieceAt = pieceat }
    
    let private PieceChange pos newPiece (bd : Brd) =
        bd
        |> PieceRemove(pos)
        |> PieceAdd pos newPiece
    
    ///Make an encoded Move(move) for this Board(bd) and return the new Board
    let MoveApply (mvi : int) (bd : Brd) =
        let move = mvi |>Move.Int2Move
        
        let mfrom = move.From
        let mto = move.To
        let piece = move.Pc
        let capture = move.CapPc
        
        let bd =
            if capture <> Piece.EMPTY then bd |> PieceRemove(mto)
            else bd
        
        let bd = bd |> PieceMove mfrom mto
        
        let bd =
            if move |> Move.IsPromotion then 
                bd |> PieceChange mto (move |> Move.Promote)
            else bd
        
        let bd =
            if move |> Move.IsCastle then 
                if piece = Piece.WKing && mto = G1 then bd |> PieceMove H1 F1
                elif piece = Piece.WKing && mto = C1 then bd |> PieceMove A1 D1
                elif piece = Piece.BKing && mto = G8 then bd |> PieceMove H8 F8
                elif piece = Piece.BKing && mto = C8 then bd |> PieceMove A8 D8
                else bd // Safety fallback
            else bd        
        
        let bd =
            if bd.CastleRights <> Castle.EMPTY then 
                if mfrom = H1 then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~Castle.WK }
                elif mfrom = A1 then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~Castle.WQ }
                elif piece = Piece.WKing then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~Castle.WK 
                                  &&& ~~~Castle.WQ }
                elif mfrom = H8 then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~Castle.BK }
                elif mfrom = A8 then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~Castle.BQ }
                elif piece = Piece.BKing then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~Castle.BK 
                                  &&& ~~~Castle.BQ }
                else bd
            else bd
        
        let bd =
            if move |> Move.IsEnPassant then 
                bd
                |> PieceRemove(Sq(mto |> Square.ToFile, 
                                  move
                                  |> Move.MovingPlayer
                                  |> Rank.MyRank(Rank.R5)))
            else bd
        
        let bd =
            if bd.EnPassant |> Square.IsInBounds then 
                { bd with EnPassant = OUTOFBOUNDS }
            else bd
        
        let bd =
            if move |> Move.IsPawnDoubleJump then 
                let ep =
                    mfrom
                    |> Square.PositionInDirectionUnsafe(move
                                                        |> Move.MovingPlayer
                                                        |> Dirn.MyNorth)
                { bd with EnPassant = ep }
            else bd
        
        let bd =
            if bd.WhosTurn = 1 then 
                { bd with Fullmove = bd.Fullmove + 1 }
            else bd
        
        let bd =
            if piece <> Piece.WPawn && piece <> Piece.BPawn 
               && capture = Piece.EMPTY then 
                { bd with Fiftymove = bd.Fiftymove + 1 }
            else { bd with Fiftymove = 0 }
        
        let bd = { bd with WhosTurn = bd.WhosTurn |> Player.PlayerOther }
        
        bd
    
    ///Create a new Board given a Fen(fen)
    let FromFEN(fen : Fen) =
        let bd = EMPTY
        
        let rec addpc posl ibd =
            if List.isEmpty posl then ibd
            else 
                let pos = posl.Head
                let pc = fen.Pieceat.[int (pos)]
                if pc = Piece.EMPTY then addpc posl.Tail ibd
                else addpc posl.Tail (ibd |> PieceAdd pos pc)
        
        let bd = addpc SQUARES bd
        { bd with CastleRights =
                      Castle.EMPTY ||| (if fen.CastleWS then 
                                              Castle.WK
                                          else Castle.EMPTY) 
                      ||| (if fen.CastleWL then Castle.WQ
                           else Castle.EMPTY) ||| (if fen.CastleBS then 
                                                         Castle.BK
                                                     else Castle.EMPTY)
                      ||| (if fen.CastleBL then Castle.BQ
                           else Castle.EMPTY)
                  WhosTurn = fen.Whosturn
                  EnPassant = fen.Enpassant
                  Fiftymove = fen.Fiftymove
                  Fullmove = fen.Fullmove }
    
    ///The starting Board at the beginning of a game
    let Start = FEN.Start |> FromFEN
