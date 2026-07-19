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
    /// simple move apply code
    let MoveApply (move : Move) (bd : Brd) =
        let mfrom = move.From
        let mto = move.To
        let piece = move.Pc
        let capture = move.CapPc
        let prom = move.Prom
        // set up new elements
        let pieceAt = bd.PieceAt|>Array.copy
        let wtKingPos = if piece = WKING then mto else bd.WtKingPos
        let mutable bkKingPos = if piece = BKING then mto else bd.BkKingPos
        let whosTurn = bd.WhosTurn |> Colour.Opp
        let mutable castleRts = bd.CastleRts
        let mutable enPassant = OUTOFBOUNDS
        let fiftyMove = if piece <> WPAWN && piece <> BPAWN && capture = EMPTY then bd.Fiftymove + 1 else 0
        let fullMove = if bd.WhosTurn = BLACK then bd.Fullmove + 1 else bd.Fullmove
        // basic changes
        pieceAt.[mfrom] <- EMPTY
        pieceAt.[mto] <- piece
        // promotion
        if move |> Move.IsPromotion then pieceAt.[mto] <- prom
        // castle
        if move |> Move.IsCastle then 
            if piece = WKING && mto = G1 then 
                pieceAt.[H1] <- EMPTY
                pieceAt.[F1] <- WROOK
            elif piece = WKING && mto = C1 then 
                pieceAt.[A1] <- EMPTY
                pieceAt.[D1] <- WROOK
            elif piece = BKING && mto = G8 then 
                pieceAt.[H8] <- EMPTY
                pieceAt.[F8] <- BROOK
            elif piece = BKING && mto = C8 then 
                pieceAt.[A8] <- EMPTY
                pieceAt.[D8] <- BROOK
        // castle rights
        if bd.CastleRts <> Castle.EMPTY then 
            if mfrom = H1 then 
                castleRts <- {castleRts with Castle.WK = false }
            elif mfrom = A1 then 
                castleRts <- {castleRts with Castle.WQ = false }
            elif piece = WKING then 
                castleRts <- {castleRts with Castle.WK = false; Castle.WQ = false }
            elif mfrom = H8 then 
                castleRts <- {castleRts with Castle.BK = false }
            elif mfrom = A8 then 
                castleRts <- {castleRts with Castle.BQ = false }
            elif piece = BKING then 
                castleRts <- {castleRts with Castle.BK = false; Castle.BQ = false }
        //en passant
        if move |> Move.IsEnPassant then 
            let epf = mto |> FL
            let epr = move |> Move.Colour |> Rank.MyRank(R5)
            let epsq = SQ(epf,epr)
            pieceAt.[epsq] <- EMPTY        
        if move |> Move.IsPawnDoubleJump then 
            let ep = mfrom |> Square.InDirn(move|>Move.Colour|>Dirn.MyNorth)
            enPassant <- ep
        { 
          PieceAt = pieceAt
          WtKingPos = wtKingPos
          BkKingPos = bkKingPos
          WhosTurn = whosTurn
          CastleRts = castleRts
          EnPassant = enPassant
          Fiftymove = fiftyMove
          Fullmove = fullMove
        }
    /// The starting Board at the beginning of a game
    let Start = FEN.StartStr |> FEN.ToBrd
