namespace Grampus

module MoveGen =
    
    let isSquareAttacked (sq : int) (attackerCol : int) (bd : Brd) =
        let file = FL sq
        let rank = RNK sq

        // 1. Knight Attacks
        let attackedByKnight =
            Dirn.AllDirectionsKnight |> Array.exists (fun dir ->
                let targetSq = sq + dir
                if targetSq >= 0 && targetSq < 64 then
                    let targetFile = FL targetSq
                    let fileDiff = abs (targetFile - file)
                    if fileDiff = 1 || fileDiff = 2 then
                        let pc = bd.[targetSq]
                        Piece.Colour pc = attackerCol && (Piece.ToPcType pc = KNIGHT)
                    else false
                else false
            )

        if attackedByKnight then true else

        // 2. King Attacks (standard step)
        let attackedByKing =
            Dirn.AllDirectionsQueen |> Array.exists (fun dir ->
                let targetSq = sq + dir
                if targetSq >= 0 && targetSq < 64 then
                    let targetFile = FL targetSq
                    if abs (targetFile - file) <= 1 then
                        let pc = bd.[targetSq]
                        Piece.Colour pc = attackerCol && (Piece.ToPcType pc = KING)
                    else false
                else false
            )

        if attackedByKing then true else

        // 3. Pawn Attacks
        // Note: If checking if a square is attacked by White, we look SOUTH (where a white pawn would come from)
        let attackedByPawn =
            if attackerCol = WHITE then
                // Check SW and SE from the square's perspective
                [ (Dirn.S + Dirn.W, file > 0); (Dirn.S + Dirn.E, file < 7) ]
                |> List.exists (fun (offset, validFile) ->
                    let targetSq = sq + offset
                    validFile && targetSq >= 0 && bd.[targetSq] = WPAWN
                )
            else
                // Check NW and NE from the square's perspective for Black pawns
                [ (Dirn.N + Dirn.W, file > 0); (Dirn.N + Dirn.E, file < 7) ]
                |> List.exists (fun (offset, validFile) ->
                    let targetSq = sq + offset
                    validFile && targetSq < 64 && bd.[targetSq] = BPAWN
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
                        let curFile = FL currentSq
                        let nextFile = FL nextSq
                        let fileDiff = abs (nextFile - curFile)
                    
                        // Standard wrap-around check
                        let isWrap = if abs dir = 8 then fileDiff <> 0 else fileDiff <> 1
                    
                        if isWrap then 
                            hitObstacle <- true
                        else
                            let pc = bd.[nextSq]
                            if pc <> EMPTY then
                                hitObstacle <- true // Hit something, stop the ray
                                if Piece.Colour pc = attackerCol && List.contains (Piece.ToPcType pc) allowedTypes then
                                    foundAttacker <- true
                            currentSq <- nextSq
                foundAttacker
            )

        // Check diagonals (Bishops/Queens)
        if checkSliding Dirn.AllDirectionsBishop [ BISHOP; QUEEN ] then true
        // Check straights (Rooks/Queens)
        elif checkSliding Dirn.AllDirectionsRook [ ROOK; QUEEN ] then true
        else false    
    let isCheck (bd: Brd) (player: int) =
        let kingSq =
            if player = WHITE then bd.WtKingPos
            else bd.BkKingPos
        let opponent = Colour.Opp player
        isSquareAttacked kingSq opponent bd
    let legal (bd : Brd) (mvs : Move list) =
        let me = bd.WhosTurn
        mvs |> List.filter (fun mv ->
            // 1. Create the board state that WOULD exist after this move
            let nbd = bd |> Board.MoveApply(mv)
            // 2. The move is legal only if 'me' is NOT in check on that new board
            let kingSq =
                if me = WHITE then nbd.WtKingPos
                else nbd.BkKingPos
            let opponent = Colour.Opp me
            not (isSquareAttacked kingSq opponent nbd) 
        )
    let wPposs (bd : Brd) (sq : int) = 
        let rank = RNK sq
        let file = FL sq
        // Helper to generate either a single move or 4 promotion moves
        let yieldPawnMoves fromSq toSq capturedPc = [
            if RNK toSq = R8 then
                yield Move.CreateProm fromSq toSq WPAWN capturedPc QUEEN
                yield Move.CreateProm fromSq toSq WPAWN capturedPc ROOK
                yield Move.CreateProm fromSq toSq WPAWN capturedPc BISHOP
                yield Move.CreateProm fromSq toSq WPAWN capturedPc KNIGHT
            else
                yield Move.Create fromSq toSq WPAWN capturedPc
        ]
        let moves = [
            // 1. Forward Moves
            let p1 = sq + Dirn.N
            if bd.[p1] = EMPTY then
                yield! yieldPawnMoves sq p1 EMPTY
                // Double Push (Only if first square was empty)
                if rank = R2 then
                    let p2 = p1 + Dirn.N
                    if bd.[p2] = EMPTY then
                        yield Move.Create sq p2 WPAWN EMPTY
            // 2. Captures (East)
            if file <> H then
                let pto = sq + Dirn.NE
                let cappc = bd.[pto]
                if cappc <> EMPTY && (Piece.Colour cappc) = BLACK then
                    yield! yieldPawnMoves sq pto cappc
                elif pto = bd.EnPassant then
                    //TODO: fix this
                    // Note: Recording BPawn as captured even though square is empty
                    yield Move.Create sq pto WPAWN EMPTY 
            // 3. Captures (West)
            if file <> A then
                let pto = sq + Dirn.NW
                let cappc = bd.[pto]
                if cappc <> EMPTY && (Piece.Colour cappc) = BLACK then
                    yield! yieldPawnMoves sq pto cappc
                elif pto = bd.EnPassant then
                    yield Move.Create sq pto WPAWN EMPTY
        ]
        moves |> legal bd  
    let bPposs (bd : Brd) (sq : int) = 
        let rank = RNK sq
        let file = FL sq
        // Helper to generate either a single move or 4 promotion moves
        let yieldPawnMoves fromSq toSq capturedPc = [
            if RNK toSq = R1 then
                yield Move.CreateProm fromSq toSq BPAWN capturedPc QUEEN
                yield Move.CreateProm fromSq toSq BPAWN capturedPc ROOK
                yield Move.CreateProm fromSq toSq BPAWN capturedPc BISHOP
                yield Move.CreateProm fromSq toSq BPAWN capturedPc KNIGHT
            else
                yield Move.Create fromSq toSq BPAWN capturedPc
        ]
        let moves = [
            // 1. Forward Moves (South)
            let p1 = sq + Dirn.S
            if bd.[p1] = EMPTY then
                yield! yieldPawnMoves sq p1 EMPTY
                // Double Push (Only from Rank 7)
                if rank = R7 then
                    let p2 = p1 + Dirn.S
                    if bd.[p2] = EMPTY then
                        yield Move.Create sq p2 BPAWN EMPTY
            // 2. Captures South-East (Diagonal Right)
            if file <> H then
                let pto = sq + Dirn.SE
                let cappc = bd.[pto]
                if cappc <> EMPTY && (Piece.Colour cappc) = WHITE then
                    yield! yieldPawnMoves sq pto cappc
                elif pto = bd.EnPassant then
                    yield Move.Create sq pto BPAWN EMPTY // Per your requirement
            // 3. Captures South-West (Diagonal Left)
            if file <> A then
                let pto = sq + Dirn.SW
                let cappc = bd.[pto]
                if cappc <> EMPTY && (Piece.Colour cappc) = WHITE then
                    yield! yieldPawnMoves sq pto cappc
                elif pto = bd.EnPassant then
                    yield Move.Create sq pto BPAWN EMPTY // Per your requirement
        ]
        moves |>  legal bd    
    let nMoves (bd: Brd) (sq: int) (myPiece: int) (enemyColor: int) =
        let startFile = FL sq
        [
            for dir in Dirn.AllDirectionsKnight do
                let targetSq = sq + dir
                if targetSq >= 0 && targetSq < 64 then
                    let targetFile = FL targetSq
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
                        if targetPc = EMPTY || (Piece.Colour targetPc) = enemyColor then
                            yield Move.Create sq targetSq myPiece targetPc
        ]    
    let wNposs bd sq = nMoves bd sq WKNIGHT BLACK |> legal bd
    let bNposs bd sq = nMoves bd sq BKNIGHT WHITE |> legal bd
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
                        let curFile = FL currentSq
                        let nextFile = FL nextSq
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
                            if targetPc = EMPTY then
                                yield Move.Create sq nextSq myPc EMPTY
                                currentSq <- nextSq
                            else
                                if (Piece.Colour targetPc) = enemyCol then
                                    yield Move.Create sq nextSq myPc targetPc
                                continueSliding <- false
        ]
    let wBposs bd sq = slidingMoves bd sq WBISHOP BLACK Dirn.AllDirectionsBishop |> legal bd
    let bBposs bd sq = slidingMoves bd sq BBISHOP WHITE Dirn.AllDirectionsBishop |> legal bd
    let wRposs bd sq = slidingMoves bd sq WROOK BLACK Dirn.AllDirectionsRook |> legal bd
    let bRposs bd sq = slidingMoves bd sq BROOK WHITE Dirn.AllDirectionsRook |> legal bd
    let wQposs bd sq = slidingMoves bd sq WQUEEN BLACK Dirn.AllDirectionsQueen |> legal bd
    let bQposs bd sq = slidingMoves bd sq WQUEEN WHITE Dirn.AllDirectionsQueen |> legal bd
    let wKposs (bd : Brd) (sq : int) =
        let startFile = FL sq
        let startRank = RNK sq
        let moves = [
            // 1. Standard Step Moves
            for dir in Dirn.AllDirectionsQueen do
                let nextSq = sq + dir
                if nextSq >= 0 && nextSq < 64 then
                    let nextFile = FL nextSq
                    // King can only move 1 file away. Prevents wrap-around.
                    if abs (nextFile - startFile) <= 1 then
                        let targetPc = bd.[nextSq]
                        if targetPc = EMPTY || (Piece.Colour targetPc) = BLACK then
                            yield Move.Create sq nextSq WKING targetPc
            // 2. Castling (White)
            if sq = 4 && startRank = R1 then
                // Kingside (O-O)
                if bd.CastleRts.WK && bd.[5] = EMPTY &&  bd.[6] = EMPTY then
                   let sqatt =
                        bd |> isSquareAttacked E1 1
                        || bd |> isSquareAttacked F1 1
                        || bd |> isSquareAttacked G1 1
                   if not sqatt then yield Move.Create sq 6 WKING EMPTY
                // Queenside (O-O-O)
                if bd.CastleRts.WQ && bd.[3] = EMPTY && bd.[2] = EMPTY && bd.[1] = EMPTY then
                   let sqatt =
                        bd |> isSquareAttacked E1 1
                        || bd |> isSquareAttacked D1 1
                        || bd |> isSquareAttacked C1 1
                   if not sqatt then yield Move.Create sq 2 WKING EMPTY
        ]
        moves |> legal bd
    let bKposs (bd : Brd) (sq : int) =
        let startFile = FL sq
        let startRank = RNK sq
        let moves = [
            // 1. Standard Step Moves
            for dir in Dirn.AllDirectionsQueen do
                let nextSq = sq + dir
                if nextSq >= 0 && nextSq < 64 then
                    let nextFile = FL nextSq
                    if abs (nextFile - startFile) <= 1 then
                        let targetPc = bd.[nextSq]
                        if targetPc = EMPTY || (Piece.Colour targetPc) = WHITE then
                            yield Move.Create sq nextSq BKING targetPc
            // 2. Castling (Black)
            if sq = 60 && startRank = R8 then
                // Kingside
                if bd.CastleRts.BK && bd.[61] = EMPTY && bd.[62] = EMPTY then
                   let sqatt =
                        bd |> isSquareAttacked E8 0
                        || bd |> isSquareAttacked F8 0
                        || bd |> isSquareAttacked G8 0
                   if not sqatt then yield Move.Create sq 62 BKING EMPTY
                // Queenside
                if bd.CastleRts.BQ && bd.[59] = EMPTY && bd.[58] = EMPTY && bd.[57] = EMPTY then
                   let sqatt =
                        bd |> isSquareAttacked E8 0
                        || bd |> isSquareAttacked D8 0
                        || bd |> isSquareAttacked C8 0
                   if not sqatt then yield Move.Create sq 58 BKING EMPTY
        ]
        moves |> legal bd    
    
    let PossMoves (bd : Brd) (sq : int) =
        let player = bd.WhosTurn
        let pc = bd.[sq]
        if player = WHITE then
            match pc with
            | WPAWN -> wPposs bd sq
            | WKNIGHT -> wNposs bd sq
            | WBISHOP -> wBposs bd sq
            | WROOK -> wRposs bd sq
            | WQUEEN -> wQposs bd sq
            | WKING-> wKposs bd sq
            | _ -> []
        else
            match pc with
            | BPAWN -> bPposs bd sq
            | BKNIGHT -> bNposs bd sq
            | BBISHOP -> bBposs bd sq
            | BROOK -> bRposs bd sq
            | BQUEEN -> bQposs bd sq
            | BKING -> bKposs bd sq
            | _ -> []
