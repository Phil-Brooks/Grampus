namespace Grampus

module MoveGenerate =
    
    // Helper to turn Square (int16) into Bitboard (Enum)
    let inline private toBB (sq: int) = 
        1UL <<< sq

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
                        if ((bd.CastleRights &&& Castle.WK) <> 0 
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
                    if ((bd.CastleRights &&& Castle.WQ) <> 0 
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
                        if ((bd.CastleRights &&& Castle.BK) <> 0 
                            && bd.PieceAt.[E8] = Piece.BKing 
                            && bd.PieceAt.[H8] = Piece.BRook && sqemp 
                            && not sqatt) then 
                            [ Move.Create E8 G8 bd.PieceAt.[E8] bd.PieceAt.[G8] ]
                        else []
                    
                    let sqatt = bd |> Board.SquareAttacked E8 0
                                || bd |> Board.SquareAttacked D8 0
                                || bd |> Board.SquareAttacked C8 0
                    let sqemp = bd.PieceAt.[B8] = Piece.EMPTY && bd.PieceAt.[C8] = Piece.EMPTY && bd.PieceAt.[D8] = Piece.EMPTY
                    if ((bd.CastleRights &&& Castle.BQ) <> 0 
                        && bd.PieceAt.[E8] = Piece.BKing 
                        && bd.PieceAt.[A8] = Piece.BRook && sqemp 
                        && not sqatt) then 
                        (Move.Create E8 C8 bd.PieceAt.[E8] bd.PieceAt.[C8]) :: mvl2
                    else mvl2
            mvl |> legal bd
    
    let private pcMoves (bd : Brd) (pt : int) (fnsqbb : int -> uint64 -> uint64) : int list =
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
                let rankBB = if me = 0 then 0xFF00UL else 0x00FF000000000000UL
                (rankBB &&& piecePositions 
                 &&& (Bitboard.shift mypawnsouth (Bitboard.shift mypawnsouth moveLocations))
                 &&& (Bitboard.shift mypawnsouth (~~~bd.PieceLocationsAll)))
                |> Bitboard.toSquares |> Array.toList |> List.map (fun piecepos ->
                    let targetpos = piecepos |> Square.PositionInDirectionUnsafe mypawnnorth |> Square.PositionInDirectionUnsafe mypawnnorth
                    Move.Create piecepos targetpos bd.PieceAt.[int piecepos] bd.PieceAt.[int targetpos]
                )

            (ptwos @ pones @ pcaps) |> legal bd

    //TODO: change to generate moves for each sq not using bitboards
    let WPposs (bd : Brd) (sq : int) = 
        let rank = Square.ToRank sq
        let file = Square.ToFile sq
        // Helper to generate either a single move or 4 promotion moves
        let yieldPawnMoves fromSq toSq capturedPc = [
            if Square.ToRank toSq = Rank.R8 then
                yield Move.CreateProm fromSq toSq Piece.WPawn capturedPc PieceType.Queen
                yield Move.CreateProm fromSq toSq Piece.WPawn capturedPc PieceType.Rook
                yield Move.CreateProm fromSq toSq Piece.WPawn capturedPc PieceType.Bishop
                yield Move.CreateProm fromSq toSq Piece.WPawn capturedPc PieceType.Knight
            else
                yield Move.Create fromSq toSq Piece.WPawn capturedPc
        ]
        let moves = [
            // 1. Forward Moves
            let p1 = sq + Dirn.N
            if bd.[p1] = Piece.EMPTY then
                yield! yieldPawnMoves sq p1 Piece.EMPTY
                // Double Push (Only if first square was empty)
                if rank = Rank.R2 then
                    let p2 = p1 + Dirn.N
                    if bd.[p2] = Piece.EMPTY then
                        yield Move.Create sq p2 Piece.WPawn Piece.EMPTY
            // 2. Captures (East)
            if file <> File.H then
                let pto = sq + Dirn.NE
                let cappc = bd.[pto]
                if cappc <> Piece.EMPTY && (Piece.colour cappc) = Player.Black then
                    yield! yieldPawnMoves sq pto cappc
                elif pto = bd.EnPassant then
                    // Note: Recording BPawn as captured even though square is empty
                    yield Move.Create sq pto Piece.WPawn Piece.EMPTY 
            // 3. Captures (West)
            if file <> File.A then
                let pto = sq + Dirn.NW
                let cappc = bd.[pto]
                if cappc <> Piece.EMPTY && (Piece.colour cappc) = Player.Black then
                    yield! yieldPawnMoves sq pto cappc
                elif pto = bd.EnPassant then
                    yield Move.Create sq pto Piece.WPawn Piece.EMPTY
        ]
        moves |> legal bd  
    let BPposs (bd : Brd) (sq : int) = 
        let rank = Square.ToRank sq
        let file = Square.ToFile sq
        // Helper to generate either a single move or 4 promotion moves
        let yieldPawnMoves fromSq toSq capturedPc = [
            if Square.ToRank toSq = Rank.R1 then
                yield Move.CreateProm fromSq toSq Piece.BPawn capturedPc PieceType.Queen
                yield Move.CreateProm fromSq toSq Piece.BPawn capturedPc PieceType.Rook
                yield Move.CreateProm fromSq toSq Piece.BPawn capturedPc PieceType.Bishop
                yield Move.CreateProm fromSq toSq Piece.BPawn capturedPc PieceType.Knight
            else
                yield Move.Create fromSq toSq Piece.BPawn capturedPc
        ]
        let moves = [
            // 1. Forward Moves (South)
            let p1 = sq + Dirn.S
            if bd.[p1] = Piece.EMPTY then
                yield! yieldPawnMoves sq p1 Piece.EMPTY
                // Double Push (Only from Rank 7)
                if rank = Rank.R7 then
                    let p2 = p1 + Dirn.S
                    if bd.[p2] = Piece.EMPTY then
                        yield Move.Create sq p2 Piece.BPawn Piece.EMPTY
            // 2. Captures South-East (Diagonal Right)
            if file <> File.H then
                let pto = sq + Dirn.SE
                let cappc = bd.[pto]
                if cappc <> Piece.EMPTY && (Piece.colour cappc) = Player.White then
                    yield! yieldPawnMoves sq pto cappc
                elif pto = bd.EnPassant then
                    yield Move.Create sq pto Piece.BPawn Piece.EMPTY // Per your requirement
            // 3. Captures South-West (Diagonal Left)
            if file <> File.A then
                let pto = sq + Dirn.SW
                let cappc = bd.[pto]
                if cappc <> Piece.EMPTY && (Piece.colour cappc) = Player.White then
                    yield! yieldPawnMoves sq pto cappc
                elif pto = bd.EnPassant then
                    yield Move.Create sq pto Piece.BPawn Piece.EMPTY // Per your requirement
        ]
        moves |>  legal bd    
    let NMoves (bd: Brd) (sq: int) (myPiece: int) (enemyColor: int) =
        let startFile = Square.ToFile sq
        [
            for dir in Dirn.AllDirectionsKnight do
                let targetSq = sq + dir
                if targetSq >= 0 && targetSq < 64 then
                    let targetFile = Square.ToFile targetSq
                    let fileDiff = targetFile - startFile
                    let isValid = 
                        match dir with
                        | Dirn.NNE | Dirn.SSE -> fileDiff = 1
                        | Dirn.EEN | Dirn.EES -> fileDiff = 2
                        | Dirn.NNW | Dirn.SSW -> fileDiff = -1
                        | Dirn.WWN | Dirn.WWS -> fileDiff = -2
                        | _ -> false
                
                    if isValid then
                        let targetPc = bd.[targetSq]
                        if targetPc = Piece.EMPTY || (Piece.colour targetPc) = enemyColor then
                            yield Move.Create sq targetSq myPiece targetPc
        ]    
    let WNposs bd sq = NMoves bd sq Piece.WKnight Player.Black |> legal bd
    let BNposs bd sq = NMoves bd sq Piece.BKnight Player.White |> legal bd
    let slidingMoves (bd: Brd) (sq: int) (myPc: int) (enemyCol: int) (dirs: int[]) =
        [
            for dir in dirs do
                let mutable currentSq = sq
                let mutable continueSliding = true
                while continueSliding do
                    let nextSq = currentSq + dir
                    if nextSq < 0 || nextSq > 63 then 
                        continueSliding <- false
                    else
                        let curFile = Square.ToFile currentSq
                        let nextFile = Square.ToFile nextSq
                        let fileDiff = abs (nextFile - curFile)
                    
                        // Logic check for wrap-around
                        // Diagonals must move 1 file; Horizontals 1 file; Verticals 0 files.
                        let isWrap = 
                            if abs dir = 8 then fileDiff <> 0 // N/S moves
                            else fileDiff <> 1               // E/W or Diagonals

                        if isWrap then 
                            continueSliding <- false
                        else
                            let targetPc = bd.[nextSq]
                            if targetPc = Piece.EMPTY then
                                yield Move.Create sq nextSq myPc Piece.EMPTY
                                currentSq <- nextSq
                            else
                                if (Piece.colour targetPc) = enemyCol then
                                    yield Move.Create sq nextSq myPc targetPc
                                continueSliding <- false
        ]
    let WBposs bd sq = slidingMoves bd sq Piece.WBishop Player.Black Dirn.AllDirectionsBishop |> legal bd
    let BBposs bd sq = slidingMoves bd sq Piece.BBishop Player.White Dirn.AllDirectionsBishop |> legal bd
    let WRposs bd sq = slidingMoves bd sq Piece.WRook Player.Black Dirn.AllDirectionsRook |> legal bd
    let BRposs bd sq = slidingMoves bd sq Piece.BRook Player.White Dirn.AllDirectionsRook |> legal bd
    let WQposs bd sq = slidingMoves bd sq Piece.WQueen Player.Black Dirn.AllDirectionsQueen |> legal bd
    let BQposs bd sq = slidingMoves bd sq Piece.WQueen Player.White Dirn.AllDirectionsQueen |> legal bd
    let WKposs (bd : Brd) (sq : int) =
        let startFile = Square.ToFile sq
        let startRank = Square.ToRank sq
        let moves = [
            // 1. Standard Step Moves
            for dir in Dirn.AllDirectionsQueen do
                let nextSq = sq + dir
                if nextSq >= 0 && nextSq < 64 then
                    let nextFile = Square.ToFile nextSq
                    // King can only move 1 file away. Prevents wrap-around.
                    if abs (nextFile - startFile) <= 1 then
                        let targetPc = bd.[nextSq]
                        if targetPc = Piece.EMPTY || (Piece.colour targetPc) = Player.Black then
                            yield Move.Create sq nextSq Piece.WKing targetPc
            // 2. Castling (White)
            if sq = 4 && startRank = Rank.R1 then
                // Kingside (O-O)
                if (bd.CastleRights &&& Castle.WK) <> 0 && bd.[5] = Piece.EMPTY &&  bd.[6] = Piece.EMPTY then
                   // TODO: need to fix this without bitboards
                   let sqatt =
                        bd |> Board.SquareAttacked E1 1
                        || bd |> Board.SquareAttacked F1 1
                        || bd |> Board.SquareAttacked G1 1
                   if not sqatt then yield Move.Create sq 6 Piece.WKing Piece.EMPTY
                // Queenside (O-O-O)
                if (bd.CastleRights &&& Castle.WQ) <> 0 && bd.[3] = Piece.EMPTY && bd.[2] = Piece.EMPTY && bd.[1] = Piece.EMPTY then
                   // TODO: need to fix this without bitboards
                   let sqatt =
                        bd |> Board.SquareAttacked E1 1
                        || bd |> Board.SquareAttacked D1 1
                        || bd |> Board.SquareAttacked C1 1
                   if not sqatt then yield Move.Create sq 2 Piece.WKing Piece.EMPTY
        ]
        moves |> legal bd
    let BKposs (bd : Brd) (sq : int) =
        let startFile = Square.ToFile sq
        let startRank = Square.ToRank sq
        let moves = [
            // 1. Standard Step Moves
            for dir in Dirn.AllDirectionsQueen do
                let nextSq = sq + dir
                if nextSq >= 0 && nextSq < 64 then
                    let nextFile = Square.ToFile nextSq
                    if abs (nextFile - startFile) <= 1 then
                        let targetPc = bd.[nextSq]
                        if targetPc = Piece.EMPTY || (Piece.colour targetPc) = Player.White then
                            yield Move.Create sq nextSq Piece.BKing targetPc
            // 2. Castling (Black)
            if sq = 60 && startRank = Rank.R8 then
                // Kingside
                if (bd.CastleRights &&& Castle.BK) <> 0 && bd.[61] = Piece.EMPTY && bd.[62] = Piece.EMPTY then
                   // TODO: need to fix this without bitboards
                   let sqatt =
                        bd |> Board.SquareAttacked E8 0
                        || bd |> Board.SquareAttacked F8 0
                        || bd |> Board.SquareAttacked G8 0
                   if not sqatt then yield Move.Create sq 62 Piece.BKing Piece.EMPTY
                // Queenside
                if (bd.CastleRights &&& Castle.BQ) <> 0 && bd.[59] = Piece.EMPTY && bd.[58] = Piece.EMPTY && bd.[57] = Piece.EMPTY then
                   // TODO: need to fix this without bitboards
                   let sqatt =
                        bd |> Board.SquareAttacked E8 0
                        || bd |> Board.SquareAttacked D8 0
                        || bd |> Board.SquareAttacked C8 0
                   if not sqatt then yield Move.Create sq 58 Piece.BKing Piece.EMPTY
        ]
        moves |> legal bd    
    
    let PossMoves (bd : Brd) (sq : int) =
        let player = bd.WhosTurn
        let pc = bd.[sq]
        if player = Player.White then
            match pc with
            | Piece.WPawn -> WPposs bd sq
            | Piece.WKnight -> WNposs bd sq
            | Piece.WBishop -> WBposs bd sq
            | Piece.WRook -> WRposs bd sq
            | Piece.WQueen -> WQposs bd sq
            | Piece.WKing -> WKposs bd sq
            | _ -> []
        else
            match pc with
            | Piece.BPawn -> BPposs bd sq
            | Piece.BKnight -> BNposs bd sq
            | Piece.BBishop -> BBposs bd sq
            | Piece.BRook -> BRposs bd sq
            | Piece.BQueen -> BQposs bd sq
            | Piece.BKing -> BKposs bd sq
            | _ -> []
