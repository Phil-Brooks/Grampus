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
            yield [| A1; A8; Dirn.N |]
            yield [| A8; A1; Dirn.S |]
            yield [| A1; H1; Dirn.E |]
            yield [| H1; A1; Dirn.W |]
            yield [| A1; H8; Dirn.NE |]
            yield [| B1; A3; Dirn.NNW |] // Knight move
        }

    // --- 1. Parsing and Basic Deconstruction ---

    [<Theory>]
    [<InlineData("a1", 0s)>]
    [<InlineData("h8", 63s)>]
    [<InlineData("e4", 28s)>]
    let ``Parse returns correct square index`` (s: string, expected: int) =
        Square.Parse s |> should equal expected

    [<Theory>]
    [<InlineData(0s, 0s, 0s)>]  // A1 -> File 0, Rank 0
    [<InlineData(63s, 7s, 7s)>] // H8 -> File 7, Rank 7
    [<InlineData(28s, 4s, 3s)>] // E4 -> File 4, Rank 3
    let ``ToRank and ToFile deconstruct correctly`` (sq: int, expectedFile: int, expectedRank: int) =
        Square.ToRank sq |> should equal expectedRank
        Square.ToFile sq |> should equal expectedFile

    // --- 2. Directional Logic ---

    [<Theory>]
    [<MemberData(nameof(DirectionTestData))>]
    let ``DirectionTo identifies correct compass direction`` (fromSq: int, toSq: int, expectedDir: int) =
        Square.DirectionTo toSq fromSq |> should equal expectedDir

    [<Fact>]
    let ``PositionInDirection returns OUTOFBOUNDS when walking off the board`` () =
        Square.PositionInDirection Dirn.N A8 |> should equal OUTOFBOUNDS
        Square.PositionInDirection Dirn.W A1 |> should equal OUTOFBOUNDS

    // --- 4. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``A square is in bounds if and only if it is between 0 and 63`` (s: int) =
        Square.IsInBounds s = (s >= 0 && s <= 63)

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Sq(ToFile(s), ToRank(s)) is identity`` (s: int) =
        if Square.IsInBounds s then
            Sq(Square.ToFile s, Square.ToRank s) = s
        else true

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Square Round-trip: ToFile and ToRank recreate the original Square`` (sq: int) =
        if Square.IsInBounds sq then
            let f = Square.ToFile sq
            let r = Square.ToRank sq
            Sq(f, r) = sq
        else true