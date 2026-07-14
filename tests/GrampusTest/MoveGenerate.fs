namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module MoveGenerate =

    let getAllLegalMoves bd =
        SQUARES |> List.collect (MoveGenerate.PossMoves bd)

    // --- 1. Starting Position (The 20-Move Rule) ---
    [<Fact>]
    let ``Starting position has exactly 20 legal moves for White`` () =
        let moves = getAllLegalMoves Board.Start
        moves.Length |> should equal 20
        // 16 Pawn moves (8 single, 8 double) + 4 Knight moves = 20

    // --- 2. Pins (Legality Check) ---
    [<Fact>]
    let ``Pinned piece cannot move off the line of fire`` () =
        // Setup: White King on E1, White Queen on E4, Black Rook on E8
        // The Queen is pinned to the King and should only be able to move along the E-file
        let fen = "4r3/8/8/8/4Q3/8/8/4K3 w - - 0 1"
        let bd = FEN.Parse fen |> Board.FromFEN
        
        let queenMoves = MoveGenerate.PossMoves bd E4
        
        // Queen can move to E2, E3, E5, E6, E7, E8 (capture)
        // She CANNOT move to D4 or F4 (this would leave king in check)
        queenMoves |> List.iter (fun m -> 
            (Move.To m |> Square.ToFile) |> should equal FileE)
        
        queenMoves.Length |> should equal 6

    // --- 3. Promotions ---
    [<Fact>]
    let ``Pawn reaching 8th rank generates 4 promotion moves`` () =
        // White Pawn on A7, nothing in the way on A8
        let fen = "8/P7/k7/8/8/8/8/4K3 w - - 0 1"
        let bd = FEN.Parse fen |> Board.FromFEN
        
        let moves = MoveGenerate.PossMoves bd A7
        
        // Should be 4 moves: A7-A8(Q), A7-A8(R), A7-A8(B), A7-A8(N)
        moves.Length |> should equal 4
        moves |> List.iter (fun m -> Move.IsPromotion m |> should be True)

    // --- 4. Castling ---
    [<Fact>]
    let ``Castling is illegal if the King must pass through check`` () =
        // White wants to castle King-side, but Black Rook controls F1
        let fen = "rnbqk2r/pppppppp/8/8/8/5r2/PPPPP1PP/RNBQK2R w KQkq - 0 1"
        let bd = FEN.Parse fen |> Board.FromFEN
        
        let kingMoves = MoveGenerate.CastleMoves bd
        // King-side (G1) should be missing because F1 is attacked by the Rook
        kingMoves |> List.exists (fun m -> Move.To m = G1) |> should be False

    // --- 5. Double Check ---
    [<Fact>]
    let ``Only King can move during a double check`` () =
        // White King is in check by Black Knight and Black Rook simultaneously
        let fen = "4r3/8/8/8/8/5n2/8/4K3 w - - 0 1"
        let bd = FEN.Parse fen |> Board.FromFEN
        
        bd.Checkers |> Bitboard.bitCount |> should equal 2
        
        let allMoves = getAllLegalMoves bd
        // All legal moves must be King moves
        allMoves |> List.iter (fun m -> 
            Move.MovingPieceType m |> should equal PieceType.King)

    // --- 6. En Passant ---
    [<Fact>]
    let ``En Passant is generated correctly`` () =
        // White just played e2-e4, Black pawn is on d4. Black can play exd3 e.p.
        let fen = "rnbqkbnr/pppp1ppp/8/8/3pP3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        let bd = FEN.Parse fen |> Board.FromFEN
        
        let pawnMoves = MoveGenerate.PossMoves bd D4
        let epMove = pawnMoves |> List.find (fun m -> Move.IsEnPassant m)
        
        Move.To epMove |> should equal E3