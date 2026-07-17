namespace Grampus

module MoveGenerate =
    
    let isSquareAttacked (sq : int) (attackerCol : int) (bd : Brd) =
        let file = Square.ToFile sq
        let rank = Square.ToRank sq

        // 1. Knight Attacks
        let attackedByKnight =
            Dirn.AllDirectionsKnight |> Array.exists (fun dir ->
                let targetSq = sq + dir
                if targetSq >= 0 && targetSq < 64 then
                    let targetFile = Square.ToFile targetSq
                    let fileDiff = abs (targetFile - file)
                    if fileDiff = 1 || fileDiff = 2 then
                        let pc = bd.[targetSq]
                        Piece.colour pc = attackerCol && (Piece.ToPieceType pc = PieceType.Knight)
                    else false
                else false
            )

        if attackedByKnight then true else

        // 2. King Attacks (standard step)
        let attackedByKing =
            Dirn.AllDirectionsQueen |> Array.exists (fun dir ->
                let targetSq = sq + dir
                if targetSq >= 0 && targetSq < 64 then
                    let targetFile = Square.ToFile targetSq
                    if abs (targetFile - file) <= 1 then
                        let pc = bd.[targetSq]
                        Piece.colour pc = attackerCol && (Piece.ToPieceType pc = PieceType.King)
                    else false
                else false
            )

        if attackedByKing then true else

        // 3. Pawn Attacks
        // Note: If checking if a square is attacked by White, we look SOUTH (where a white pawn would come from)
        let attackedByPawn =
            if attackerCol = Player.White then
                // Check SW and SE from the square's perspective
                [ (Dirn.S + Dirn.W, file > 0); (Dirn.S + Dirn.E, file < 7) ]
                |> List.exists (fun (offset, validFile) ->
                    let targetSq = sq + offset
                    validFile && targetSq >= 0 && bd.[targetSq] = Piece.WPawn
                )
            else
                // Check NW and NE from the square's perspective for Black pawns
                [ (Dirn.N + Dirn.W, file > 0); (Dirn.N + Dirn.E, file < 7) ]
                |> List.exists (fun (offset, validFile) ->
                    let targetSq = sq + offset
                    validFile && targetSq < 64 && bd.[targetSq] = Piece.BPawn
                )

        if attackedByPawn then true else

        // 4. Sliding Attacks (Bishops, Rooks, Queens)
        let checkSliding (dirs: int[]) (allowedTypes: int list) =
            dirs |> Array.exists (fun dir ->
                let mutable currentSq = sq
                let mutable foundAttacker = false
                let mutable hitObstacle = false
            
                while not hitObstacle do
                    let nextSq = currentSq + dir
                    if nextSq < 0 || nextSq > 63 then 
                        hitObstacle <- true
                    else
                        let curFile = Square.ToFile currentSq
                        let nextFile = Square.ToFile nextSq
                        let fileDiff = abs (nextFile - curFile)
                    
                        // Standard wrap-around check
                        let isWrap = if abs dir = 8 then fileDiff <> 0 else fileDiff <> 1
                    
                        if isWrap then 
                            hitObstacle <- true
                        else
                            let pc = bd.[nextSq]
                            if pc <> Piece.EMPTY then
                                hitObstacle <- true // Hit something, stop the ray
                                if Piece.colour pc = attackerCol && List.contains (Piece.ToPieceType pc) allowedTypes then
                                    foundAttacker <- true
                            currentSq <- nextSq
                foundAttacker
            )

        // Check diagonals (Bishops/Queens)
        if checkSliding Dirn.AllDirectionsBishop [ PieceType.Bishop; PieceType.Queen ] then true
        // Check straights (Rooks/Queens)
        elif checkSliding Dirn.AllDirectionsRook [ PieceType.Rook; PieceType.Queen ] then true
        else false    
    let isCheck (bd: Brd) (player: int) =
        let kingSq =
            if player = Player.White then bd.WtKingPos
            else bd.BkKingPos
        let opponent = Player.PlayerOther player
        isSquareAttacked kingSq opponent bd
    let legal (bd : Brd) (mvs : int list) =
        let me = bd.WhosTurn
        mvs |> List.filter (fun mv ->
            // 1. Create the board state that WOULD exist after this move
            let nbd = bd |> Board.MoveApply(mv)
            // 2. The move is legal only if 'me' is NOT in check on that new board
            let kingSq =
                if me = Player.White then nbd.WtKingPos
                else nbd.BkKingPos
            let opponent = Player.PlayerOther me
            not (isSquareAttacked kingSq opponent nbd) 
        )
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
                   let sqatt =
                        bd |> isSquareAttacked E1 1
                        || bd |> isSquareAttacked F1 1
                        || bd |> isSquareAttacked G1 1
                   if not sqatt then yield Move.Create sq 6 Piece.WKing Piece.EMPTY
                // Queenside (O-O-O)
                if (bd.CastleRights &&& Castle.WQ) <> 0 && bd.[3] = Piece.EMPTY && bd.[2] = Piece.EMPTY && bd.[1] = Piece.EMPTY then
                   let sqatt =
                        bd |> isSquareAttacked E1 1
                        || bd |> isSquareAttacked D1 1
                        || bd |> isSquareAttacked C1 1
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
                   let sqatt =
                        bd |> isSquareAttacked E8 0
                        || bd |> isSquareAttacked F8 0
                        || bd |> isSquareAttacked G8 0
                   if not sqatt then yield Move.Create sq 62 Piece.BKing Piece.EMPTY
                // Queenside
                if (bd.CastleRights &&& Castle.BQ) <> 0 && bd.[59] = Piece.EMPTY && bd.[58] = Piece.EMPTY && bd.[57] = Piece.EMPTY then
                   let sqatt =
                        bd |> isSquareAttacked E8 0
                        || bd |> isSquareAttacked D8 0
                        || bd |> isSquareAttacked C8 0
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
