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
        bd.WhosTurn |> should equal 0
        bd.PieceAt.[int E1] |> should equal WKING
        bd.PieceAt.[int E8] |> should equal BKING
        bd.Fullmove |> should equal 1
        bd.Fiftymove |> should equal 0

    // --- 2. Move Application (Simple & Capture) ---

    [<Fact>]
    let ``MoveApply: Simple pawn move updates board correctly`` () =
        // e2 -> e4
        let mv = Move.Create E2 E4 WPAWN EMPTY
        let nextBd = Board.MoveApply mv Board.Start
        
        nextBd.PieceAt.[int E2] |> should equal EMPTY
        nextBd.PieceAt.[int E4] |> should equal WPAWN
        nextBd.WhosTurn |> should equal 1

    [<Fact>]
    let ``MoveApply: Capture removes piece`` () =
        // Setup: White Knight on F3 captures Black Pawn on E5
        let bd = Board.EMP 
                 |> Board.pieceAdd F3 WKNIGHT 
                 |> Board.pieceAdd E5 BPAWN
        let mv = Move.Create F3 E5 WKNIGHT BPAWN
        let nextBd = Board.MoveApply mv bd
        
        nextBd.PieceAt.[int E5] |> should equal WKNIGHT

    // --- 3. Castling & Special Rules ---

    [<Fact>]
    let ``MoveApply: White King Side Castle moves both King and Rook`` () =
        // Setup a position where White can castle
        let fenStr = "rnbqk2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1"
        let bd = FEN.ToBrd fenStr
        let mv = Move.Create E1 G1 WKING EMPTY // O-O
        let nextBd = Board.MoveApply mv bd
        
        nextBd.PieceAt.[G1] |> should equal WKING
        nextBd.PieceAt.[F1] |> should equal WROOK
        nextBd.CastleRts.WK |> should equal false

    [<Fact>]
    let ``MoveApply: En Passant removes the correct pawn`` () =
        // White Pawn on E5, Black Pawn on D5, EP square is D6
        let bd = { Board.EMP with EnPassant = D6; WhosTurn = 0 }
                 |> Board.pieceAdd E5 WPAWN
                 |> Board.pieceAdd D5 BPAWN
        let mv = Move.CreateEp E5 D6 WPAWN BPAWN // exd6 e.p.
        let nextBd = Board.MoveApply mv bd
        
        nextBd.PieceAt.[int D5] |> should equal EMPTY


    // --- 5. Property Based Testing (Invariants) ---



    [<Fact>]
    let ``MoveApply: Moving a piece on a kingless board does not crash`` () =
        // Setup: Just two rooks, no kings
        let bd = Board.EMP 
                 |> Board.pieceAdd A1 WROOK 
                 |> Board.pieceAdd A8 BROOK
        let mv = Move.Create A1 A5 WROOK EMPTY
        
        // This should not throw an exception
        let nextBd = Board.MoveApply mv bd
        
        nextBd.PieceAt.[int A5] |> should equal WROOK
