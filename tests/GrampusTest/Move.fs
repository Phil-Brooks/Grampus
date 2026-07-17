namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

module Move =

    // --- 1. Test Data for Bit-Packing Theories ---
    let MoveTestData : obj array seq =
        seq {
            // [| from; to; movingPiece; capturedPiece |]
            yield [| E2; E4; Piece.WPawn; Piece.EMPTY |]
            yield [| B1; C3; Piece.WKnight; Piece.EMPTY |]
            yield [| D7; D5; Piece.BPawn; Piece.EMPTY |]
            yield [| F3; E5; Piece.WKnight; Piece.BPawn |] // Capture
        }

    // --- 2. Functional Tests ---

    [<Theory>]
    [<MemberData(nameof(MoveTestData))>]
    let ``Move.Create correctly encodes and decodes basic move data`` (f, t, p, c) =
        let mv = Move.Create f t p c
        
        mv.From |> should equal f
        mv.To |> should equal t
        mv.Pc |> should equal p
        mv.CapPc |> should equal c
        Move.IsCapture mv |> should equal (c <> Piece.EMPTY)
        Move.IsPromotion mv |> should be False

    [<Fact>]
    let ``Move.CreateProm correctly encodes promotion data`` () =
        // White Pawn on A7 captures on B8 and promotes to Queen
        let f, t, p, c, prom = A7, B8, Piece.WPawn, Piece.BRook, PieceType.Queen
        let mv = Move.CreateProm f t p c prom
        
        mv.From |> should equal f
        mv.To |> should equal t
        mv.Pc |> should equal p
        Move.IsPromotion mv |> should be True
        mv.Prom |> should equal prom
        Move.Promote mv |> should equal Piece.WQueen

    [<Fact>]
    let ``IsCastle identifies king moves of two squares`` () =
        // White King E1 to G1
        let mv = Move.Create E1 G1 Piece.WKing Piece.EMPTY
        Move.IsCastle mv |> should be True
        
        // White King E1 to F1 (Not a castle)
        let mv2 = Move.Create E1 F1 Piece.WKing Piece.EMPTY
        Move.IsCastle mv2 |> should be False

    [<Fact>]
    let ``IsEnPassant identifies pawn diagonal moves without captures`` () =
        // White Pawn capturing at E6 from D5 (EP) 
        // Note: In your logic, EP is a diagonal move where CapturedPiece is EMPTY
        let mv = Move.Create D5 E6 Piece.WPawn Piece.EMPTY
        Move.IsEnPassant mv |> should be True

    [<Fact>]
    let ``IsPawnDoubleJump identifies 16 square vertical moves`` () =
        let mv = Move.Create E2 E4 Piece.WPawn Piece.EMPTY
        Move.IsPawnDoubleJump mv |> should be True

    // --- 3. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<ChessDimGenerator>; typeof<PieceGenerator> |])>]
    let ``Move deconstruction is always the inverse of Move creation`` (f: int) (t: int) (p: int) =
        // Only test valid squares (0-63)
        if Square.IsInBounds f && Square.IsInBounds t then
            let mv = Move.Create f t p Piece.EMPTY
            mv.From = f && mv.To = t && mv.Pc = p
        else true

    [<Property(Arbitrary = [| typeof<PieceGenerator> |])>]
    let ``MovingPlayer matches the color of the moving piece`` (p: int) =
        if p <> Piece.EMPTY then
            let mv = Move.Create E2 E4 p Piece.EMPTY
            let player = Move.MovingPlayer mv
            Piece.PieceToPlayer p = Some player
        else true