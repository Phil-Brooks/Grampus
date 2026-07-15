namespace Grampus

module Board =
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
        
        let posBits =
            (mfrom |> Square.ToBitboard) ||| (mto |> Square.ToBitboard)
        
        let piecetypes =
            bd.PieceTypes
            |> List.mapi (fun i p -> 
                   if i = int (pieceType) then p ^^^ posBits
                   else p)
        
        let wtprbds =
            if player = 0 then bd.WtPrBds ^^^ posBits
            else bd.WtPrBds
        
        let bkprbds =
            if player = 1 then bd.BkPrBds ^^^ posBits
            else bd.BkPrBds
        
        let pieceLocationsAll = bd.PieceLocationsAll ^^^ posBits
        
        let wtkingpos =
            if pieceType = PieceType.King && player = 0 then mto
            else bd.WtKingPos
        
        let bkkingpos =
            if pieceType = PieceType.King && player = 1 then mto
            else bd.BkKingPos
        
        { bd with PieceAt = pieceat
                  PieceTypes = piecetypes
                  WtPrBds = wtprbds
                  BkPrBds = bkprbds
                  PieceLocationsAll = pieceLocationsAll
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
        
        let posBits = pos |> Square.ToBitboard
        
        let piecetypes =
            bd.PieceTypes
            |> List.mapi (fun i p -> 
                   if i = int (piece |> Piece.ToPieceType) then p ||| posBits
                   else p)
        
        let piecelocationsall = bd.PieceLocationsAll ||| posBits
        
        let wtprbds =
            if (piece |> Piece.PieceToPlayer).Value = 0 then 
                bd.WtPrBds ||| posBits
            else bd.WtPrBds
        
        let bkprbds =
            if (piece |> Piece.PieceToPlayer).Value = 1 then 
                bd.BkPrBds ||| posBits
            else bd.BkPrBds
        
        let wtkingpos =
            if pieceType = PieceType.King && player = 0 then pos
            else bd.WtKingPos
        
        let bkkingpos =
            if pieceType = PieceType.King && player = 1 then pos
            else bd.BkKingPos
        
        { bd with PieceAt = pieceat
                  PieceTypes = piecetypes
                  PieceLocationsAll = piecelocationsall
                  WtPrBds = wtprbds
                  BkPrBds = bkprbds
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
        
        let notPosBits = ~~~(pos |> Square.ToBitboard)
        
        let piecetypes =
            bd.PieceTypes
            |> List.mapi (fun i p -> 
                   if i = int (pieceType) then p &&& notPosBits
                   else p)
        
        let wtprbds =
            if player = 0 then bd.WtPrBds &&& notPosBits
            else bd.WtPrBds
        
        let bkprbds =
            if player = 1 then bd.BkPrBds &&& notPosBits
            else bd.BkPrBds
        
        let piecelocationsall = bd.PieceLocationsAll &&& notPosBits
        { bd with PieceAt = pieceat
                  PieceTypes = piecetypes
                  WtPrBds = wtprbds
                  BkPrBds = bkprbds
                  PieceLocationsAll = piecelocationsall }
    
    let private PieceChange pos newPiece (bd : Brd) =
        bd
        |> PieceRemove(pos)
        |> PieceAdd pos newPiece
    
    let private AttacksToBoth (mto : int) (bd : Brd) =
        (Attacks.KnightAttacks(mto) &&& bd.PieceTypes.[int (PieceType.Knight)]) 
        ||| ((Attacks.RookAttacks mto bd.PieceLocationsAll) 
             &&& (bd.PieceTypes.[int (PieceType.Queen)] 
                  ||| bd.PieceTypes.[int (PieceType.Rook)])) 
        ||| ((Attacks.BishopAttacks mto bd.PieceLocationsAll) 
             &&& (bd.PieceTypes.[int (PieceType.Queen)] 
                  ||| bd.PieceTypes.[int (PieceType.Bishop)])) 
        ||| (Attacks.KingAttacks(mto) &&& (bd.PieceTypes.[int (PieceType.King)])) 
        ||| ((Attacks.PawnAttacks mto 1) &&& bd.BkPrBds 
             &&& bd.PieceTypes.[int (PieceType.Pawn)]) 
        ||| ((Attacks.PawnAttacks mto 0) &&& bd.WtPrBds 
             &&& bd.PieceTypes.[int (PieceType.Pawn)])
    
    ///Gets the Bitboard that defines the squares that attack the specified Square(mto) by the specified Player(by) for this Board(bd) 
    let AttacksTo (mto : int) (by : int) (bd : Brd) =
        bd
        |> AttacksToBoth(mto)
        &&& (if by = 0 then bd.WtPrBds
             else bd.BkPrBds)
    
    ///Is the Square(mto) attacked by the specified Player(by) for this Board(bd)
    let SquareAttacked (mto : int) (by : int) (bd : Brd) =
        bd
        |> AttacksTo mto by
        <> Bitboard.Empty
    
    ///Make an encoded Move(move) for this Board(bd) and return the new Board
    let MoveApply (move : int) (bd : Brd) =
        let mfrom = move |> Move.From
        let mto = move |> Move.To
        let piece = move |> Move.MovingPiece
        let capture = move |> Move.CapturedPiece
        
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
            if bd.CastleRights <> CstlFlgs.EMPTY then 
                if mfrom = H1 then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~CstlFlgs.WhiteShort }
                elif mfrom = A1 then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~CstlFlgs.WhiteLong }
                elif piece = Piece.WKing then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~CstlFlgs.WhiteShort 
                                  &&& ~~~CstlFlgs.WhiteLong }
                elif mfrom = H8 then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~CstlFlgs.BlackShort }
                elif mfrom = A8 then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~CstlFlgs.BlackLong }
                elif piece = Piece.BKing then 
                    { bd with CastleRights =
                                  bd.CastleRights &&& ~~~CstlFlgs.BlackShort 
                                  &&& ~~~CstlFlgs.BlackLong }
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
        
                // 1. Switch turns
        let bd = { bd with WhosTurn = bd.WhosTurn |> Player.PlayerOther }
        
        // 2. Determine the current King's position safely
        let kingPos = 
            if bd.WhosTurn = 0 then bd.WtKingPos 
            else bd.BkKingPos

        // 3. Update Checkers ONLY if the king is actually on the board
        let checkers =
            if kingPos = OUTOFBOUNDS then 
                Bitboard.Empty
            else
                // Find pieces of the PREVIOUS player that attack the CURRENT king
                let opponent = bd.WhosTurn |> Player.PlayerOther
                let opponentPieces = 
                    if opponent = 0 then bd.WtPrBds 
                    else bd.BkPrBds
                
                (bd |> AttacksToBoth kingPos) &&& opponentPieces

        { bd with Checkers = checkers }
    
    ///Is there a check on the Board(bd)
    let IsChk(bd : Brd) = bd.Checkers <> Bitboard.Empty
    
    ///Is there a check on Player(kingplayer) on the Board(bd)
    let IsChck (kingplayer : int) (bd : Brd) =
        let kingpos =
            if kingplayer = 0 then bd.WtKingPos
            else bd.BkKingPos
        // Guard: If the king isn't on the board, he can't be in check
        if kingpos = OUTOFBOUNDS then false
        else bd |> SquareAttacked kingpos (kingplayer |> Player.PlayerOther)
    
    ///Create a new Board given a Fen(fen)
    let FromFEN(fen : Fen) =
        let bd = BrdEMP
        
        let rec addpc posl ibd =
            if List.isEmpty posl then ibd
            else 
                let pos = posl.Head
                let pc = fen.Pieceat.[int (pos)]
                if pc = Piece.EMPTY then addpc posl.Tail ibd
                else addpc posl.Tail (ibd |> PieceAdd pos pc)
        
        let bd = addpc SQUARES bd
        { bd with CastleRights =
                      CstlFlgs.EMPTY ||| (if fen.CastleWS then 
                                              CstlFlgs.WhiteShort
                                          else CstlFlgs.EMPTY) 
                      ||| (if fen.CastleWL then CstlFlgs.WhiteLong
                           else CstlFlgs.EMPTY) ||| (if fen.CastleBS then 
                                                         CstlFlgs.BlackShort
                                                     else CstlFlgs.EMPTY)
                      ||| (if fen.CastleBL then CstlFlgs.BlackLong
                           else CstlFlgs.EMPTY)
                  WhosTurn = fen.Whosturn
                  EnPassant = fen.Enpassant
                  Fiftymove = fen.Fiftymove
                  Fullmove = fen.Fullmove
                  Checkers =
                      bd
                      |> AttacksToBoth(if fen.Whosturn = 0 then 
                                           bd.WtKingPos
                                       else bd.BkKingPos)
                      &&& (if fen.Whosturn = 1 then bd.WtPrBds
                           else bd.BkPrBds) }
    
    ///The starting Board at the beginning of a game
    let Start = FEN.Start |> FromFEN
