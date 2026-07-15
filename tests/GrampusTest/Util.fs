namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open Grampus

module Util =

    // --- 1. Casting Helpers (Sanity Checks) ---
    [<Theory>]
    [<InlineData(1, PieceType.Pawn)>]
    [<InlineData(6, PieceType.King)>]
    [<InlineData(0, PieceType.EMPTY)>]
    let ``PcTp casts integers to PieceType correctly`` (i: int, expected: PieceType) =
        PcTp i |> should equal expected

    [<Theory>]
    [<InlineData(1, Piece.WPawn)>]
    [<InlineData(14, Piece.BKing)>]
    [<InlineData(0, Piece.EMPTY)>]
    let ``Pc casts integers to Piece correctly`` (i: int, expected: Piece) =
        Pc i |> should equal expected

    [<Fact>]
    let ``BitB correctly handles uint64 flags`` () =
        BitB 1UL |> should equal Bitboard.A1
        BitB 255UL |> should equal Bitboard.Rank1
        BitB 0UL |> should equal Bitboard.Empty

    // --- 2. Arithmetic Operators ---
    [<Fact>]
    let ``Rank arithmetic operators work correctly`` () =
        Rank1 +! 1s |> should equal Rank2
        Rank8 -! 7s |> should equal Rank1
        Rank4 +! 2s |> should equal Rank6

    [<Fact>]
    let ``File arithmetic operators work correctly`` () =
        FileA ++ 1s |> should equal FileB
        FileH -- 7s |> should equal FileA
        FileC ++ 3s |> should equal FileF

    // --- 3. Property Based Testing (FsCheck) ---
    // Using the ChessDimGenerator we created for TypesTests
    
    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Rank addition and subtraction are inverses`` (r: Rank) (offset: int16) =
        // We use a small offset to stay within reasonable bounds for the logic
        let smallOffset = offset % 4s 
        (r +! smallOffset) -! smallOffset = r

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``File addition and subtraction are inverses`` (f: File) (offset: int16) =
        let smallOffset = offset % 4s
        (f ++ smallOffset) -- smallOffset = f

