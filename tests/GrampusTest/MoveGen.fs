namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module MoveGenerate =

    let getAllLegalMoves bd =
        SQUARES |> List.collect (MoveGen.PossMoves bd)

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
        let bd = FEN.ToBrd fen
        
        let queenMoves = MoveGen.PossMoves bd E4
        
        // Queen can move to E2, E3, E5, E6, E7, E8 (capture)
        // She CANNOT move to D4 or F4 (this would leave king in check)
        queenMoves |> List.iter (fun m -> 
            (m.To |> FL) |> should equal E)
        
        queenMoves.Length |> should equal 6

    // --- 3. Promotions ---
    [<Fact>]
    let ``Pawn reaching 8th rank generates 4 promotion moves`` () =
        // White Pawn on A7, nothing in the way on A8
        let fen = "8/P7/k7/8/8/8/8/4K3 w - - 0 1"
        let bd = FEN.ToBrd fen
        
        let moves = MoveGen.PossMoves bd A7
        
        // Should be 4 moves: A7-A8(Q), A7-A8(R), A7-A8(B), A7-A8(N)
        moves.Length |> should equal 4
        moves |> List.iter (fun m -> Move.IsProm m |> should be True)

    // --- 5. Double Check ---
    [<Fact>]
    let ``Only King can move during a double check`` () =
        // White King is in check by Black Knight and Black Rook simultaneously
        let fen = "4r3/8/8/8/8/5n2/8/4K3 w - - 0 1"
        let bd = FEN.ToBrd fen
        
        let allMoves = getAllLegalMoves bd
        // All legal moves must be King moves
        allMoves |> List.iter (fun m -> 
            m.Pc |> should equal WKING)

    // --- 6. En Passant ---
    [<Fact>]
    let ``En Passant is generated correctly`` () =
        // White just played e2-e4, Black pawn is on d4. Black can play exd3 e.p.
        let fen = "rnbqkbnr/pppp1ppp/8/8/3pP3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        let bd = FEN.ToBrd fen
        
        let pawnMoves = MoveGen.PossMoves bd D4
        let epMove = pawnMoves |> List.find (fun m -> Move.IsEP bd m)
        
        epMove.To |> should equal E3

    [<Fact>]
    let ``Pawn correctly attacks diagonal squares but not forward`` () =
        // Black pawn on d5
        let fen = "8/8/8/3p4/8/8/8/4K3 w - - 0 1"
        let bd = FEN.ToBrd fen
        
        // Squares d5 attacks (c4 and e4)
        MoveGen.isSquareAttacked C4 BLACK bd |> should be True
        MoveGen.isSquareAttacked E4 BLACK bd |> should be True
        
        // Square directly in front is NOT attacked by a pawn
        MoveGen.isSquareAttacked D4 BLACK bd |> should be False

    [<Fact>]
    let ``Knight attacks jumping over pieces`` () =
        // Black Knight on c3, White pawns surrounding it
        let fen = "8/8/8/8/8/2n5/1PPP4/1PKP4 w - - 0 1"
        let bd = FEN.ToBrd fen
        
        // Knight should jump over the pawns to attack e4 and b1
        MoveGen.isSquareAttacked E4 BLACK bd |> should be True
        MoveGen.isSquareAttacked B1 BLACK bd |> should be True
        
        // Random square
        MoveGen.isSquareAttacked H8 BLACK bd |> should be False

    [<Fact>]
    let ``Sliding attacks are blocked by intervening pieces`` () =
        // Black Rook on a8, White King on a1
        let fen = "r7/8/8/8/8/8/8/K7 w - - 0 1"
        let bd1 = FEN.ToBrd fen
        MoveGen.isSquareAttacked A1 BLACK bd1 |> should be True

        // Now place a blocker on a5
        let fenBlocked = "r7/8/8/p7/8/8/8/K7 w - - 0 1"
        let bd2 = FEN.ToBrd fenBlocked
        
        // The Rook no longer attacks A1 because the pawn is in the way
        MoveGen.isSquareAttacked A1 BLACK bd2 |> should be False

    [<Fact>]
    let ``Knight attack does not wrap around the board edge`` () =
        // White Knight on h1 (Index 7)
        let fen = "k7/8/8/K7/8/8/8/7N w - - 0 1"
        let bd = FEN.ToBrd fen
        
        // Square f2 is a legal jump
        MoveGen.isSquareAttacked F2 WHITE bd |> should be True
        
        // Square a2 (Index 8) is 15 away from h1. 
        // 7 + 15 = 22, but it's on a different side of the board.
        // It should NOT be attacked.
        MoveGen.isSquareAttacked A2 WHITE bd |> should be False

    [<Fact>]
    let ``Bishop attacks correctly on diagonals`` () =
        // White Bishop on d4
        let fen = "8/8/8/8/3B4/8/8/4k3 b - - 0 1"
        let bd = FEN.ToBrd fen
        MoveGen.isSquareAttacked G7 WHITE bd |> should be True
        MoveGen.isSquareAttacked A1 WHITE bd |> should be True
        MoveGen.isSquareAttacked D5 WHITE bd |> should be False // Straight line

    [<Fact>]
    let ``King attacks adjacent squares`` () =
        // White King on e1
        let fen = "8/8/8/8/8/8/8/4K3 w - - 0 1"
        let bd = FEN.ToBrd fen
        
        MoveGen.isSquareAttacked D1 WHITE bd |> should be True
        MoveGen.isSquareAttacked D2 WHITE bd |> should be True
        MoveGen.isSquareAttacked E2 WHITE bd |> should be True
        MoveGen.isSquareAttacked E3 WHITE bd |> should be False // Too far