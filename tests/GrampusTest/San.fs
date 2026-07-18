namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module San =

    // Helper to create a minimal board state
    let emptyBoard = {
        PieceAt = Array.create 64 0
        WtKingPos = E1; BkKingPos = E8
        WhosTurn = 0
        CastleRts = { WK = true; WQ = true; BK = true; BQ = true }
        EnPassant = OUTOFBOUNDS
        Fiftymove = 0; Fullmove = 1
    }

    // Helper to create a Move
    let makeMove fromSq toSq pc cap prom =
        { From = fromSq; To = toSq; Pc = pc; CapPc = cap; Prom = prom }

    [<Theory>]
    // Standard Piece Moves
    [<InlineData(G1, F3, 2, 0, 0, "Nf3")>] // White Knight (2)
    [<InlineData(C1, F4, 3, 0, 0, "Bf4")>] // White Bishop (3)
    [<InlineData(D1, D4, 5, 0, 0, "Qd4")>] // White Queen (5)
    // Standard Pawn Moves
    [<InlineData(E2, E4, 1, 0, 0, "e4")>]  // White Pawn (1)
    [<InlineData(E7, E5, 9, 0, 0, "e5")>]  // Black Pawn (9)
    let ``ToSan handles standard non-capture moves`` (fromSq, toSq, pc, cap, prom, expected) =
        let m = makeMove fromSq toSq pc cap prom
        San.ToSan emptyBoard m |> should equal expected

    [<Theory>]
    // Piece Captures
    [<InlineData(F3, E5, 2, 9, 0, "Nxe5")>] // Knight takes Pawn
    [<InlineData(B5, C6, 3, 10, 0, "Bxc6")>] // Bishop takes Knight
    // Pawn Captures
    [<InlineData(E4, D5, 1, 9, 0, "exd5")>] // Pawn takes Pawn
    let ``ToSan handles captures correctly`` (fromSq, toSq, pc, cap, prom, expected) =
        let m = makeMove fromSq toSq pc cap prom
        San.ToSan emptyBoard m |> should equal expected

    [<Theory>]
    // White Castling
    [<InlineData(E1, G1, 6, 0, 0, "O-O")>]   // King (6)
    [<InlineData(E1, C1, 6, 0, 0, "O-O-O")>]
    // Black Castling
    [<InlineData(E8, G8, 14, 0, 0, "O-O")>]  // King (14)
    [<InlineData(E8, C8, 14, 0, 0, "O-O-O")>]
    let ``ToSan handles castling`` (fromSq, toSq, pc, cap, prom, expected) =
        let m = makeMove fromSq toSq pc cap prom
        San.ToSan emptyBoard m |> should equal expected

    [<Theory>]
    // White Promotions
    [<InlineData(A7, A8, 1, 0, 5, "a8=Q")>] // Pawn to Queen
    [<InlineData(B7, B8, 1, 0, 2, "b8=N")>] // Pawn to Knight
    // Black Promotions with Capture
    [<InlineData(H2, G1, 9, 2, 5, "hxg1=Q")>] // Pawn takes Knight and promotes
    let ``ToSan handles promotions`` (fromSq, toSq, pc, cap, prom, expected) =
        let m = makeMove fromSq toSq pc cap prom
        San.ToSan emptyBoard m |> should equal expected

    [<Fact>]
    let ``sqToAlg converts indices to algebraic notation`` () =
        San.sqToAlg A1 |> should equal "a1"
        San.sqToAlg H8 |> should equal "h8"
        San.sqToAlg E4 |> should equal "e4"