namespace Grampus

module Board =
    let EMPTY =
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
        let player = (piece |> Piece.ToColour).Value
        let pieceType = piece |> Piece.ToPcType
        
        let pieceat =
            bd.PieceAt
            |> Array.mapi (fun i p -> 
                   if i = int (mto) then piece
                   elif i = int (mfrom) then Piece.EMPTY
                   else p)
        
        let wtkingpos =
            if pieceType = PcType.King && player = 0 then mto
            else bd.WtKingPos
        
        let bkkingpos =
            if pieceType = PcType.King && player = 1 then mto
            else bd.BkKingPos
        
        { bd with PieceAt = pieceat
                  WtKingPos = wtkingpos
                  BkKingPos = bkkingpos }
    let pieceAdd pos (piece : int) (bd : Brd) =
        let player = (piece |> Piece.ToColour).Value
        let pieceType = piece |> Piece.ToPcType
        
        let pieceat =
            bd.PieceAt
            |> Array.mapi (fun i p -> 
                   if i = int (pos) then piece
                   else p)
        
        let wtkingpos =
            if pieceType = PcType.King && player = 0 then pos
            else bd.WtKingPos
        
        let bkkingpos =
            if pieceType = PcType.King && player = 1 then pos
            else bd.BkKingPos
        
        { bd with PieceAt = pieceat
                  WtKingPos = wtkingpos
                  BkKingPos = bkkingpos }
    let pieceRemove (pos : int) (bd : Brd) =
        let piece = bd.PieceAt.[pos]
        let player = (piece |> Piece.ToColour).Value
        let pieceType = piece |> Piece.ToPcType
        
        let pieceat =
            bd.PieceAt
            |> Array.mapi (fun i p -> 
                   if i = pos then Piece.EMPTY
                   else p)
        
        { bd with PieceAt = pieceat }
    let pieceChange pos newPiece (bd : Brd) =
        bd
        |> pieceRemove(pos)
        |> pieceAdd pos newPiece
    ///Make an encoded Move(move) for this Board(bd) and return the new Board
    let MoveApply (move : Move) (ibd : Brd) =
        let mfrom = move.From
        let mto = move.To
        let piece = move.Pc
        let capture = move.CapPc
        let mutable bd = ibd
        if capture <> Piece.EMPTY then bd <- bd |> pieceRemove(mto)
        bd <- bd |> pieceMove mfrom mto
        if move |> Move.IsPromotion then 
            bd <- bd |> pieceChange mto (move |> Move.Promote)
        if move |> Move.IsCastle then 
            if piece = Piece.WKing && mto = G1 then bd <- bd |> pieceMove H1 F1
            elif piece = Piece.WKing && mto = C1 then bd <- bd |> pieceMove A1 D1
            elif piece = Piece.BKing && mto = G8 then bd <- bd |> pieceMove H8 F8
            elif piece = Piece.BKing && mto = C8 then bd <- bd |> pieceMove A8 D8
        if bd.CastleRts <> Castle.EMPTY then 
            if mfrom = H1 then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.WK = false }}
            elif mfrom = A1 then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.WQ = false }}
            elif piece = Piece.WKing then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.WK = false; Castle.WQ = false }}
            elif mfrom = H8 then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.BK = false }}
            elif mfrom = A8 then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.BQ = false }}
            elif piece = Piece.BKing then 
                bd <- { bd with CastleRts = {bd.CastleRts with Castle.BK = false; Castle.BQ = false }}
        if move |> Move.IsEnPassant then 
            let epf = mto |> Square.ToFile
            let epr = move |> Move.Colour |> Rank.MyRank(Rank.R5)
            let epsq = Sq(epf,epr)
            bd <- bd |> pieceRemove epsq        
        if bd.EnPassant |> Square.InBounds then 
            bd <- { bd with EnPassant = OUTOFBOUNDS }
        if move |> Move.IsPawnDoubleJump then 
            let ep = mfrom |> Square.InDirn(move|>Move.Colour|>Dirn.MyNorth)
            bd <- { bd with EnPassant = ep }
        if bd.WhosTurn = 1 then 
            bd <- { bd with Fullmove = bd.Fullmove + 1 }
        if piece <> Piece.WPawn && piece <> Piece.BPawn && capture = Piece.EMPTY then 
            bd <- { bd with Fiftymove = bd.Fiftymove + 1 }
        else bd <- { bd with Fiftymove = 0 }
        bd <- { bd with WhosTurn = bd.WhosTurn |> Colour.Opp }
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
                else addpc posl.Tail (ibd |> pieceAdd pos pc)
        let bd = addpc SQUARES bd
        { bd with CastleRts =
                              {
                                WK = fen.CastleWK
                                WQ = fen.CastleWQ
                                BK = fen.CastleBK
                                BQ = fen.CastleBQ
                              }
                  WhosTurn = fen.Whosturn
                  EnPassant = fen.Enpassant
                  Fiftymove = fen.Fiftymove
                  Fullmove = fen.Fullmove }
    ///The starting Board at the beginning of a game
    let Start = FEN.Start |> FromFEN
