namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

module Board =

    // --- 1. Initialization ---

    [<Fact>]
    let ``Board Start position is correctly initialized`` () =
        let bd = Board.Start
        bd.WhosTurn |> should equal Player.White
        bd.PieceAt.[int E1] |> should equal Piece.WKing
        bd.PieceAt.[int E8] |> should equal Piece.BKing
        bd.Fullmove |> should equal 1
        bd.Fiftymove |> should equal 0
        // Starting position has 32 pieces
        Bitboard.bitCount bd.PieceLocationsAll |> should equal 32

    // --- 2. Move Application (Simple & Capture) ---

    [<Fact>]
    let ``MoveApply: Simple pawn move updates board correctly`` () =
        // e2 -> e4
        let mv = Move.Create E2 E4 Piece.WPawn Piece.EMPTY
        let nextBd = Board.MoveApply mv Board.Start
        
        nextBd.PieceAt.[int E2] |> should equal Piece.EMPTY
        nextBd.PieceAt.[int E4] |> should equal Piece.WPawn
        nextBd.WhosTurn |> should equal Player.Black
        // Verify Bitboards
        Bitboard.containsPos E2 nextBd.PieceLocationsAll |> should be False
        Bitboard.containsPos E4 nextBd.PieceLocationsAll |> should be True

    [<Fact>]
    let ``MoveApply: Capture removes piece and updates bitboards`` () =
        // Setup: White Knight on F3 captures Black Pawn on E5
        let bd = BrdEMP 
                 |> Board.PieceAdd F3 Piece.WKnight 
                 |> Board.PieceAdd E5 Piece.BPawn
        let mv = Move.Create F3 E5 Piece.WKnight Piece.BPawn
        let nextBd = Board.MoveApply mv bd
        
        nextBd.PieceAt.[int E5] |> should equal Piece.WKnight
        Bitboard.bitCount nextBd.BkPrBds |> should equal 0
        Bitboard.bitCount nextBd.WtPrBds |> should equal 1

    // --- 3. Castling & Special Rules ---

    [<Fact>]
    let ``MoveApply: White King Side Castle moves both King and Rook`` () =
        // Setup a position where White can castle
        let fenStr = "rnbqk2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1"
        let bd = FEN.Parse fenStr |> Board.FromFEN
        let mv = Move.Create E1 G1 Piece.WKing Piece.EMPTY // O-O
        let nextBd = Board.MoveApply mv bd
        
        nextBd.PieceAt.[int G1] |> should equal Piece.WKing
        nextBd.PieceAt.[int F1] |> should equal Piece.WRook
        nextBd.CastleRights.HasFlag(CstlFlgs.WhiteShort) |> should be False

    [<Fact>]
    let ``MoveApply: En Passant removes the correct pawn`` () =
        // White Pawn on E5, Black Pawn on D5, EP square is D6
        let bd = { BrdEMP with EnPassant = D6; WhosTurn = Player.White }
                 |> Board.PieceAdd E5 Piece.WPawn
                 |> Board.PieceAdd D5 Piece.BPawn
        let mv = Move.Create E5 D6 Piece.WPawn Piece.EMPTY // exd6 e.p.
        let nextBd = Board.MoveApply mv bd
        
        nextBd.PieceAt.[int D5] |> should equal Piece.EMPTY
        Bitboard.containsPos D5 nextBd.PieceLocationsAll |> should be False

    // --- 4. Checks & Attacks ---

    [<Fact>]
    let ``IsChck correctly identifies king is under fire`` () =
        // White King on E1, Black Rook on E8
        let bd = BrdEMP 
                 |> Board.PieceAdd E1 Piece.WKing 
                 |> Board.PieceAdd E8 Piece.BRook
        
        Board.IsChck Player.White bd |> should be True
        Board.IsChck Player.Black bd |> should be False

    // --- 5. Property Based Testing (Invariants) ---

    [<Property(Arbitrary = [| typeof<PieceGenerator>; typeof<ChessDimGenerator> |])>]
    let ``PieceLocationsAll is always the sum of White and Black bitboards`` (sq: Square) (p: Piece) =
        if Square.IsInBounds sq && p <> Piece.EMPTY then
            let bd = Board.PieceAdd sq p BrdEMP
            bd.PieceLocationsAll = (bd.WtPrBds ||| bd.BkPrBds)
        else true

    [<Property(Arbitrary = [| typeof<PieceGenerator>; typeof<ChessDimGenerator> |])>]
    let ``Adding then removing a piece returns board to empty`` (sq: Square) (p: Piece) =
        if Square.IsInBounds sq && p <> Piece.EMPTY then
            let bd = BrdEMP |> Board.PieceAdd sq p |> Board.PieceRemove sq
            bd.PieceLocationsAll = Bitboard.Empty && bd.PieceAt.[int sq] = Piece.EMPTY
        else true

    [<Fact>]
    let ``MoveApply: Moving a piece on a kingless board does not crash`` () =
        // Setup: Just two rooks, no kings
        let bd = BrdEMP 
                 |> Board.PieceAdd A1 Piece.WRook 
                 |> Board.PieceAdd A8 Piece.BRook
        let mv = Move.Create A1 A5 Piece.WRook Piece.EMPTY
        
        // This should not throw an exception
        let nextBd = Board.MoveApply mv bd
        
        nextBd.PieceAt.[int A5] |> should equal Piece.WRook
        nextBd.Checkers |> should equal Bitboard.Empty
