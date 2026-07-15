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
    let ``Parse returns correct internal index for valid characters`` (c: char, expected: int) =
        Rank.fromChar c |> should equal expected

    [<Fact>]
    let ``Parse throws exception for invalid rank characters`` () =
        (fun () -> Rank.fromChar '9' |> ignore) |> should throw typeof<System.Exception>
        (fun () -> Rank.fromChar 'z' |> ignore) |> should throw typeof<System.Exception>

    [<Theory>]
    [<InlineData(0s, "1")>]
    [<InlineData(7s, "8")>]
    let ``RankToString returns correct chess notation`` (rank: int, expected: string) =
        Rank.RankToString rank |> should equal expected

    // --- 3. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Rank Round-trip: Parse(RankToString(r)) equals r`` (r: int) =
        // Property: Converting a rank to a string and back to a rank should be an identity function
        let s = Rank.RankToString r
        Rank.fromChar s.[0] = r

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``IsInBounds returns true for all generated valid ranks`` (r: int) =
        Rank.IsInBounds r = true

    [<Property>]
    let ``IsInBounds returns false for clearly invalid ranks`` (i: int) =
        if i < 0 || i > 7 then
            Rank.IsInBounds i = false
        else true

