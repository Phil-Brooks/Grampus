namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

module Square =

    let DirectionTestData : obj array seq =
        seq {
            yield [| A1; A8; Dirn.DirN |]
            yield [| A8; A1; Dirn.DirS |]
            yield [| A1; H1; Dirn.DirE |]
            yield [| H1; A1; Dirn.DirW |]
            yield [| A1; H8; Dirn.DirNE |]
            yield [| B1; A3; Dirn.DirNNW |] // Knight move
        }

    // --- 1. Parsing and Basic Deconstruction ---

    [<Theory>]
    [<InlineData("a1", 0s)>]
    [<InlineData("h8", 63s)>]
    [<InlineData("e4", 28s)>]
    let ``Parse returns correct square index`` (s: string, expected: Square) =
        Square.Parse s |> should equal expected

    [<Theory>]
    [<InlineData(0s, 0s, 0s)>]  // A1 -> File 0, Rank 0
    [<InlineData(63s, 7s, 7s)>] // H8 -> File 7, Rank 7
    [<InlineData(28s, 4s, 3s)>] // E4 -> File 4, Rank 3
    let ``ToRank and ToFile deconstruct correctly`` (sq: Square, expectedFile: int, expectedRank: Rank) =
        Square.ToRank sq |> should equal expectedRank
        Square.ToFile sq |> should equal expectedFile

    // --- 2. Directional Logic ---

    [<Theory>]
    [<MemberData(nameof(DirectionTestData))>]
    let ``DirectionTo identifies correct compass direction`` (fromSq: Square, toSq: Square, expectedDir: Dirn) =
        Square.DirectionTo toSq fromSq |> should equal expectedDir

    [<Fact>]
    let ``PositionInDirection returns OUTOFBOUNDS when walking off the board`` () =
        Square.PositionInDirection Dirn.DirN A8 |> should equal OUTOFBOUNDS
        Square.PositionInDirection Dirn.DirW A1 |> should equal OUTOFBOUNDS

    // --- 3. Geometric Logic (Between) ---

    [<Fact>]
    let ``Between returns correct bitmask for squares in between`` () =
        // Between A1 and A4 should be A2 and A3
        let bb = Square.Between A4 A1
        let expected = (1UL <<< int A2) ||| (1UL <<< int A3) |> BitB
        bb |> should equal expected

    [<Fact>]
    let ``Between returns Empty for adjacent squares or non-linear paths`` () =
        Square.Between A2 A1 |> should equal Bitboard.Empty
        Square.Between B3 A1 |> should equal Bitboard.Empty // Knight move has no "between"

    // --- 4. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``A square is in bounds if and only if it is between 0 and 63`` (s: int) =
        Square.IsInBounds s = (s >= 0 && s <= 63)

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Sq(ToFile(s), ToRank(s)) is identity`` (s: Square) =
        if Square.IsInBounds s then
            Sq(Square.ToFile s, Square.ToRank s) = s
        else true

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``ToBitboard popcount is always 1 for in-bounds squares`` (s: Square) =
        if Square.IsInBounds s then
            let bb = Square.ToBitboard s
            System.Numerics.BitOperations.PopCount(uint64 bb) = 1
        else
            Square.ToBitboard s = Bitboard.Empty

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Square Round-trip: ToFile and ToRank recreate the original Square`` (sq: Square) =
        if Square.IsInBounds sq then
            let f = Square.ToFile sq
            let r = Square.ToRank sq
            Sq(f, r) = sq
        else true