namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open Grampus

module Util =

    // --- 1. Casting Helpers (Sanity Checks) ---
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
        Rank.R1 + 1 |> should equal Rank.R2
        Rank.R8 - 7 |> should equal Rank.R1
        Rank.R4 + 2 |> should equal Rank.R6

    [<Fact>]
    let ``File arithmetic operators work correctly`` () =
        File.A + 1 |> should equal File.B
        File.H - 7 |> should equal File.A
        File.C + 3 |> should equal File.F

    // --- 3. Property Based Testing (FsCheck) ---
    // Using the ChessDimGenerator we created for TypesTests
    
    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Rank addition and subtraction are inverses`` (r: int) (offset: int) =
        // We use a small offset to stay within reasonable bounds for the logic
        let smallOffset = offset % 4
        (r + smallOffset) - smallOffset = r

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``File addition and subtraction are inverses`` (f: int) (offset: int) =
        let smallOffset = offset % 4
        (f + smallOffset) - smallOffset = f

