namespace Grampus

module MoveGenerate =
    
    // Helper to turn Square (int16) into Bitboard (Enum)
    let inline private toBB (sq: int) = 
        LanguagePrimitives.EnumOfValue<uint64, Bitboard>(1UL <<< sq)

    let private legal (bd : Brd) (mvs : int list) =
        let me = bd.WhosTurn
        let rec filt (imvl : int list) omvl =
            match imvl with
            | [] -> omvl |> List.rev
            | mv :: tail ->
                let nbd = bd |> Board.MoveApply(mv)
                let inchk : bool = (nbd |> Board.IsChck me)
                if inchk then filt tail omvl
                else filt tail (mv :: omvl)
        filt mvs []
    
    let KingMoves(bd : Brd) : int list =
        let me = bd.WhosTurn
        let targetLocations =
            (if me = 1 then bd.WtPrBds else bd.BkPrBds)
            ||| (~~~bd.PieceLocationsAll)
        
        let kingPos = if me = 0 then bd.WtKingPos else bd.BkKingPos
        
        let rec getKingAttacks att mvl =
            if att = Bitboard.Empty then mvl
            else 
                let attPos, natt = Bitboard.popFirst(att)
                let mv = Move.Create kingPos attPos bd.PieceAt.[int kingPos] bd.PieceAt.[int attPos]
                getKingAttacks natt (mv :: mvl)
        
        let attacks = Attacks.KingAttacks(kingPos) &&& targetLocations
        let mvl = getKingAttacks attacks []
        mvl |> legal bd
    
    let CastleMoves(bd : Brd) : int list =
        let checkerCount = bd.Checkers |> Bitboard.bitCount
        if (checkerCount > 0) || (bd |> Board.IsChk) then []
        else 
            let mvl =
                if bd.WhosTurn = 0 then 
                    let mvl1 =
                        let sqatt =
                            bd |> Board.SquareAttacked E1 1
                            || bd |> Board.SquareAttacked F1 1
                            || bd |> Board.SquareAttacked G1 1
                        let sqemp =
                            bd.PieceAt.[int F1] = Piece.EMPTY 
                            && bd.PieceAt.[int G1] = Piece.EMPTY
                        if (bd.CastleRights.HasFlag(CstlFlgs.WhiteShort) 
                            && bd.PieceAt.[int E1] = Piece.WKing 
                            && bd.PieceAt.[int H1] = Piece.WRook && sqemp 
                            && not sqatt) then 
                            [ Move.Create E1 G1 bd.PieceAt.[int E1] bd.PieceAt.[int G1] ]
                        else []
                    
                    let sqatt = bd |> Board.SquareAttacked E1 1
                                || bd |> Board.SquareAttacked D1 1
                                || bd |> Board.SquareAttacked C1 1
                    let sqemp =
                        bd.PieceAt.[B1] = Piece.EMPTY 
                        && bd.PieceAt.[C1] = Piece.EMPTY 
                        && bd.PieceAt.[D1] = Piece.EMPTY
                    if (bd.CastleRights.HasFlag(CstlFlgs.WhiteLong) 
                        && bd.PieceAt.[E1] = Piece.WKing 
                        && bd.PieceAt.[A1] = Piece.WRook && sqemp 
                        && not sqatt) then 
                        (Move.Create E1 C1 bd.PieceAt.[E1] bd.PieceAt.[C1]) :: mvl1
                    else mvl1
                else 
                    // Black Castling Logic...
                    let mvl2 =
                        let sqatt = bd |> Board.SquareAttacked E8 0
                                    || bd |> Board.SquareAttacked F8 0
                                    || bd |> Board.SquareAttacked G8 0
                        let sqemp = bd.PieceAt.[F8] = Piece.EMPTY && bd.PieceAt.[G8] = Piece.EMPTY
                        if (bd.CastleRights.HasFlag(CstlFlgs.BlackShort) 
                            && bd.PieceAt.[E8] = Piece.BKing 
                            && bd.PieceAt.[H8] = Piece.BRook && sqemp 
                            && not sqatt) then 
                            [ Move.Create E8 G8 bd.PieceAt.[E8] bd.PieceAt.[G8] ]
                        else []
                    
                    let sqatt = bd |> Board.SquareAttacked E8 0
                                || bd |> Board.SquareAttacked D8 0
                                || bd |> Board.SquareAttacked C8 0
                    let sqemp = bd.PieceAt.[B8] = Piece.EMPTY && bd.PieceAt.[C8] = Piece.EMPTY && bd.PieceAt.[D8] = Piece.EMPTY
                    if (bd.CastleRights.HasFlag(CstlFlgs.BlackLong) 
                        && bd.PieceAt.[E8] = Piece.BKing 
                        && bd.PieceAt.[A8] = Piece.BRook && sqemp 
                        && not sqatt) then 
                        (Move.Create E8 C8 bd.PieceAt.[E8] bd.PieceAt.[C8]) :: mvl2
                    else mvl2
            mvl |> legal bd
    
    let private pcMoves (bd : Brd) (pt : int) (fnsqbb : int -> Bitboard -> Bitboard) : int list =
        let me = bd.WhosTurn
        let kingPos = if me = 0 then bd.WtKingPos else bd.BkKingPos
        
        let targetLocations =
            let checkerCount = bd.Checkers |> Bitboard.bitCount
            if checkerCount = 1 then 
                let checkerPos = bd.Checkers |> Bitboard.getFirstPos
                let evasionTargets = (kingPos |> Square.Between(checkerPos)) ||| (toBB checkerPos)
                ((if me = 1 then bd.WtPrBds else bd.BkPrBds) ||| (~~~bd.PieceLocationsAll)) &&& evasionTargets
            else 
                (if me = 1 then bd.WtPrBds else bd.BkPrBds) ||| (~~~bd.PieceLocationsAll)
        
        let rec getAttacks psns imvl =
            if psns = Bitboard.Empty || targetLocations = Bitboard.Empty then imvl
            else 
                let piecepos, npsns = Bitboard.popFirst(psns)
                let piece = bd.PieceAt.[int piecepos]
                let atts = (fnsqbb piecepos bd.PieceLocationsAll) &&& targetLocations
                
                let rec getAtts att jmvl =
                    if att = Bitboard.Empty then jmvl
                    else 
                        let attPos, natt = Bitboard.popFirst(att)
                        let mv = Move.Create piecepos attPos piece bd.PieceAt.[int attPos]
                        getAtts natt (mv :: jmvl)
                
                getAttacks npsns (getAtts atts imvl)
        
        let piecePositions = (if me = 0 then bd.WtPrBds else bd.BkPrBds) &&& bd.PieceTypes.[int pt]
        getAttacks piecePositions [] |> legal bd

    // (Simplified logic for other MoveTo functions using toSquares)
    let KnightMovesTo (mto : int) (bd : Brd) =
        let atts = Attacks.KnightAttacks mto
        let piecePositions = (if bd.WhosTurn = 0 then bd.WtPrBds else bd.BkPrBds) &&& bd.PieceTypes.[PieceType.Knight]
        let pieceposs = (atts &&& piecePositions) |> Bitboard.toSquares
        pieceposs |> Array.toList |> List.map (fun p -> Move.Create p mto bd.PieceAt.[int p] bd.PieceAt.[int mto]) |> legal bd

    let KnightMoves(bd : Brd) =
        if (bd.Checkers |> Bitboard.bitCount) > 1 then []
        else pcMoves bd PieceType.Knight (fun pp _ -> Attacks.KnightAttacks pp)

    let BishopMoves(bd : Brd) =
        if (bd.Checkers |> Bitboard.bitCount) > 1 then []
        else pcMoves bd PieceType.Bishop (fun pp bb -> Attacks.BishopAttacks pp bb)

    let RookMoves(bd : Brd) =
        if (bd.Checkers |> Bitboard.bitCount) > 1 then []
        else pcMoves bd PieceType.Rook (fun pp bb -> Attacks.RookAttacks pp bb)

    let QueenMoves(bd : Brd) =
        if (bd.Checkers |> Bitboard.bitCount) > 1 then []
        else pcMoves bd PieceType.Queen (fun pp bb -> Attacks.QueenAttacks pp bb)

    // PAWN LOGIC UPDATED
    let PawnMoves(bd : Brd) =
        let checkerCount = bd.Checkers |> Bitboard.bitCount
        if checkerCount > 1 then []
        else 
            let me = bd.WhosTurn
            let mypawnwest = if me = 0 then Dirn.NW else Dirn.SW
            let mypawneast = if me = 0 then Dirn.NE else Dirn.SE
            let mypawnnorth = if me = 0 then Dirn.N else Dirn.S
            let mypawnsouth = if me = 0 then Dirn.S else Dirn.N
            let myrank8 = if me = 0 then Rank.R8 else Rank.R1
            let myrank2 = if me = 0 then Rank.R2 else Rank.R7
            
            let kingPos = if me = 0 then bd.WtKingPos else bd.BkKingPos
            
            let evasionTargets =
                if checkerCount = 1 then 
                    let checkerPos = bd.Checkers |> Bitboard.getFirstPos
                    (kingPos |> Square.Between(checkerPos)) ||| (toBB checkerPos)
                else ~~~Bitboard.Empty
            
            let piecePositions = (if me = 0 then bd.WtPrBds else bd.BkPrBds) &&& bd.PieceTypes.[int PieceType.Pawn]
            let captureLocations = if me = 1 then bd.WtPrBds else bd.BkPrBds
            let targLocations = (captureLocations &&& evasionTargets) ||| (if bd.EnPassant <> OUTOFBOUNDS then toBB bd.EnPassant else Bitboard.Empty)
            
            let moveLocations = (~~~bd.PieceLocationsAll) &&& evasionTargets

            // 1. Captures
            let getPcaps capDir att =
                att |> Bitboard.toSquares |> Array.toList |> List.collect (fun targetpos ->
                    let piecepos = targetpos |> Square.PositionInDirectionUnsafe (capDir |> Dirn.Opposite)
                    if (targetpos / 8) = myrank8 then 
                        [ PieceType.Queen; PieceType.Rook; PieceType.Bishop; PieceType.Knight ]
                        |> List.map (fun p -> Move.CreateProm piecepos targetpos bd.PieceAt.[int piecepos] bd.PieceAt.[int targetpos] p)
                    else [ Move.Create piecepos targetpos bd.PieceAt.[int piecepos] bd.PieceAt.[int targetpos] ]
                )

            let pcaps = 
                (getPcaps mypawneast ((Bitboard.shift mypawneast piecePositions) &&& targLocations)) @
                (getPcaps mypawnwest ((Bitboard.shift mypawnwest piecePositions) &&& targLocations))

            // 2. Single Pushes
            let pones = 
                ((Bitboard.shift mypawnsouth moveLocations) &&& piecePositions)
                |> Bitboard.toSquares |> Array.toList |> List.collect (fun piecepos ->
                    let targetpos = piecepos |> Square.PositionInDirectionUnsafe mypawnnorth
                    if (targetpos / 8) = myrank8 then
                        [ PieceType.Queen; PieceType.Rook; PieceType.Bishop; PieceType.Knight ]
                        |> List.map (fun p -> Move.CreateProm piecepos targetpos bd.PieceAt.[int piecepos] bd.PieceAt.[int targetpos] p)
                    else [ Move.Create piecepos targetpos bd.PieceAt.[int piecepos] bd.PieceAt.[int targetpos] ]
                )

            // 3. Double Pushes
            let ptwos =
                let rankBB = LanguagePrimitives.EnumOfValue<uint64, Bitboard>(if me = 0 then 0xFF00UL else 0x00FF000000000000UL)
                (rankBB &&& piecePositions 
                 &&& (Bitboard.shift mypawnsouth (Bitboard.shift mypawnsouth moveLocations))
                 &&& (Bitboard.shift mypawnsouth (~~~bd.PieceLocationsAll)))
                |> Bitboard.toSquares |> Array.toList |> List.map (fun piecepos ->
                    let targetpos = piecepos |> Square.PositionInDirectionUnsafe mypawnnorth |> Square.PositionInDirectionUnsafe mypawnnorth
                    Move.Create piecepos targetpos bd.PieceAt.[int piecepos] bd.PieceAt.[int targetpos]
                )

            (ptwos @ pones @ pcaps) |> legal bd

    let PossMoves (bd : Brd) (sq : int) =
        let pc = bd.[sq]
        match pc |> Piece.PieceToPlayer with
        | Some p when p = bd.WhosTurn ->
            match pc |> Piece.ToPieceType with
            | PieceType.Pawn -> bd |> PawnMoves |> List.filter (fun m -> Move.From m = sq)
            | PieceType.Knight -> bd |> KnightMoves |> List.filter (fun m -> Move.From m = sq)
            | PieceType.Bishop -> bd |> BishopMoves |> List.filter (fun m -> Move.From m = sq)
            | PieceType.Rook -> bd |> RookMoves |> List.filter (fun m -> Move.From m = sq)
            | PieceType.Queen -> bd |> QueenMoves |> List.filter (fun m -> Move.From m = sq)
            | PieceType.King -> (KingMoves bd @ CastleMoves bd) |> List.filter (fun m -> Move.From m = sq)
            | _ -> []
        | _ -> []