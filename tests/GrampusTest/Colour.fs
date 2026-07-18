namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

type PlayerGenerator =
    static member Player() =
        FsCheck.FSharp.Gen.elements [ 0; 1 ] |> Arb.fromGen

module Player =
    // 1. Create a static property that returns a sequence of object arrays
    let PerspectiveData : obj array seq =
        seq {
            yield [| R1; 0; R1 |]
            yield [| R8; 0; R8 |]
            yield [| R1; 1; R8 |]
            yield [| R2; 1; R7 |]
            yield [| R7; 1; R2 |]
            yield [| R8; 1; R1 |]
        }

    // --- 1. Constant & Collection Verification ---
    [<Fact>]
    let ``AllPlayers contains exactly White and Black`` () =
        Colour.All.Length |> should equal 2
        Colour.All |> should contain 0
        Colour.All |> should contain 1

    // --- 2. Player Flipping Logic ---
    [<Theory>]
    [<InlineData(0, 1)>]
    [<InlineData(1, 0)>]
    let ``PlayerOther returns the opponent`` (input: int, expected: int) =
        Colour.Opp input |> should equal expected

    // --- 3. Perspective / MyRank Logic ---
    [<Theory>]
    [<MemberData(nameof(PerspectiveData))>]
    let ``MyRank respects player perspective correctly`` (rank: int, player: int, expected: int) =
        Rank.MyRank rank player |> should equal expected

    // --- 4. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<PlayerGenerator> |])>]
    let ``PlayerOther is its own inverse`` (p: int) =
        // Applying PlayerOther twice should return the original player
        p |> Colour.Opp |> Colour.Opp = p

    [<Property(Arbitrary = [| typeof<PlayerGenerator> |])>]
    let ``PlayerOther never returns the same player`` (p: int) =
        p |> Colour.Opp <> p

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``MyRank for White is always equal to the input rank`` (r: int) =
        Rank.MyRank r 0 = r

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``MyRank for Black and White together sums to 7`` (r: int) =
        // Rank indices are 0-7. Rank 1 (0) for White + Rank 1 (7) for Black = 7.
        int (Rank.MyRank r 0) + int (Rank.MyRank r 1) = 7