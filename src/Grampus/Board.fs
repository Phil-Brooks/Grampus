namespace Grampus

module Board =
    let EMP =
        { PieceAt = Array.create 64 0
          WtKingPos = OUTOFBOUNDS
          BkKingPos = OUTOFBOUNDS
          WhosTurn = 0
          CastleRts = Castle.EMPTY
          EnPassant = OUTOFBOUNDS
          Fiftymove = 0
          Fullmove = 0 }
    let pieceMove (mfrom : int) mto (bd : Brd) =
        let piece = bd.PieceAt.[mfrom]
        let pieceAt = bd.PieceAt|>Array.copy
        let mutable wtKingPos = bd.WtKingPos
        let mutable bkKingPos = bd.BkKingPos
        pieceAt.[mfrom] <- EMPTY
        pieceAt.[mto] <- piece
        if piece = WKING then wtKingPos <- mto
        if piece = BKING then bkKingPos <- mto
        { bd with PieceAt = pieceAt; WtKingPos = wtKingPos; BkKingPos = bkKingPos }
    let pieceAdd mto (piece : int) (bd : Brd) =
        let pieceAt = bd.PieceAt|>Array.copy
        let mutable wtKingPos = bd.WtKingPos
        let mutable bkKingPos = bd.BkKingPos
        pieceAt.[mto] <- piece
        if piece = WKING then wtKingPos <- mto
        if piece = BKING then bkKingPos <- mto
        { bd with PieceAt = pieceAt; WtKingPos = wtKingPos; BkKingPos = bkKingPos }
    let pieceRemove mfrom (bd : Brd) =
        let piece = bd.PieceAt.[mfrom]
        let pieceAt = bd.PieceAt|>Array.copy
        let mutable wtKingPos = bd.WtKingPos
        let mutable bkKingPos = bd.BkKingPos
        pieceAt.[mfrom] <- EMPTY
        if piece = WKING then wtKingPos <- OUTOFBOUNDS
        if piece = BKING then bkKingPos <- OUTOFBOUNDS
        { bd with PieceAt = pieceAt; WtKingPos = wtKingPos; BkKingPos = bkKingPos }
    let pieceChange mfrom newPiece (bd : Brd) =
        bd
        |> pieceRemove mfrom
        |> pieceAdd mfrom newPiece
    ///Make an encoded Move(move) for this Board(bd) and return the new Board
    let MoveApply (move : Move) (ibd : Brd) =
        let mfrom = move.From
        let mto = move.To
        let piece = move.Pc
        let capture = move.CapPc
        let mutable bd = ibd
        if capture <> EMPTY then bd <- bd |> pieceRemove(mto)
        bd <- bd |> pieceMove mfrom mto
        if move |> Move.IsPromotion then 
            bd <- bd |> pieceChange mto move.Prom
        if move |> Move.IsCastle then 
            if piece = WKING&& mto = G1 then bd <- bd |> pieceMove H1 F1
            elif piece = WKING&& mto = C1 then bd <- bd |> pieceMove A1 D1
            elif piece = BKING && mto = G8 then bd <- bd |> pieceMove H8 F8
            elif piece = BKING && mto = C8 then bd <- bd |> pieceMove A8 D8
        if bd.CastleRts <> Castle.EMPTY then 
            if mfrom = H1 then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.WK = false }}
            elif mfrom = A1 then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.WQ = false }}
            elif piece = WKING then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.WK = false; Castle.WQ = false }}
            elif mfrom = H8 then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.BK = false }}
            elif mfrom = A8 then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.BQ = false }}
            elif piece = BKING then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.BK = false; Castle.BQ = false }}
        if move |> Move.IsEnPassant then 
            let epf = mto |> FL
            let epr = move |> Move.Colour |> Rank.MyRank(R5)
            let epsq = SQ(epf,epr)
            bd <- bd |> pieceRemove epsq        
        if bd.EnPassant |> Square.InBounds then 
            bd <- { bd with EnPassant = OUTOFBOUNDS }
        if move |> Move.IsPawnDoubleJump then 
            let ep = mfrom |> Square.InDirn(move|>Move.Colour|>Dirn.MyNorth)
            bd <- { bd with EnPassant = ep }
        if bd.WhosTurn = 1 then 
            bd <- { bd with Fullmove = bd.Fullmove + 1 }
        if piece <> WPAWN && piece <> BPAWN && capture = EMPTY then 
            bd <- { bd with Fiftymove = bd.Fiftymove + 1 }
        else bd <- { bd with Fiftymove = 0 }
        bd <- { bd with WhosTurn = bd.WhosTurn |> Colour.Opp }
        bd
    ///The starting Board at the beginning of a game
    let Start = FEN.StartStr |> FEN.ToBrd
