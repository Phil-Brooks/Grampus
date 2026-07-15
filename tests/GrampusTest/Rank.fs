namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

module Rank =

    // --- 1. Parsing and String Conversion ---

    [<Theory>]
    [<InlineData('1', 0s)>] // Rank1
    [<InlineData('8', 7s)>] // Rank8
    [<InlineData('4', 3s)>] // Rank4
    let ``Parse returns correct internal index for valid characters`` (c: char, expected: Rank) =
        Rank.Parse c |> should equal expected

    [<Fact>]
    let ``Parse throws exception for invalid rank characters`` () =
        (fun () -> Rank.Parse '9' |> ignore) |> should throw typeof<System.Exception>
        (fun () -> Rank.Parse 'z' |> ignore) |> should throw typeof<System.Exception>

    [<Theory>]
    [<InlineData(0s, "1")>]
    [<InlineData(7s, "8")>]
    let ``RankToString returns correct chess notation`` (rank: Rank, expected: string) =
        Rank.RankToString rank |> should equal expected

    // --- 2. Bitboard Mapping ---

    [<Theory>]
    [<InlineData(0s, Bitboard.Rank1)>]
    [<InlineData(7s, Bitboard.Rank8)>]
    let ``ToBitboard returns the correct bitmask for the rank`` (rank: Rank, expected: Bitboard) =
        Rank.ToBitboard rank |> should equal expected

    [<Fact>]
    let ``ToBitboard returns Empty for out of bounds rank`` () =
        Rank.ToBitboard 8 |> should equal Bitboard.Empty

    // --- 3. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Rank Round-trip: Parse(RankToString(r)) equals r`` (r: Rank) =
        // Property: Converting a rank to a string and back to a rank should be an identity function
        let s = Rank.RankToString r
        Rank.Parse s.[0] = r

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``IsInBounds returns true for all generated valid ranks`` (r: Rank) =
        Rank.IsInBounds r = true

    [<Property>]
    let ``IsInBounds returns false for clearly invalid ranks`` (i: int) =
        if i < 0 || i > 7 then
            Rank.IsInBounds i = false
        else true

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Rank Bitboard always has 8 bits set`` (r: Rank) =
        let bb = Rank.ToBitboard r
        System.Numerics.BitOperations.PopCount(uint64 bb) = 8