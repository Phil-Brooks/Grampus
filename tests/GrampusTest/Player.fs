namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

type PlayerGenerator =
    static member Player() =
        FsCheck.FSharp.Gen.elements [ Player.White; Player.Black ] |> Arb.fromGen

module Player =
    // 1. Create a static property that returns a sequence of object arrays
    let PerspectiveData : obj array seq =
        seq {
            yield [| Rank1; Player.White; Rank1 |]
            yield [| Rank8; Player.White; Rank8 |]
            yield [| Rank1; Player.Black; Rank8 |]
            yield [| Rank2; Player.Black; Rank7 |]
            yield [| Rank7; Player.Black; Rank2 |]
            yield [| Rank8; Player.Black; Rank1 |]
        }

    // --- 1. Constant & Collection Verification ---
    [<Fact>]
    let ``AllPlayers contains exactly White and Black`` () =
        Player.AllPlayers.Length |> should equal 2
        Player.AllPlayers |> should contain Player.White
        Player.AllPlayers |> should contain Player.Black

    // --- 2. Player Flipping Logic ---
    [<Theory>]
    [<InlineData(Player.White, Player.Black)>]
    [<InlineData(Player.Black, Player.White)>]
    let ``PlayerOther returns the opponent`` (input: Player, expected: Player) =
        Player.PlayerOther input |> should equal expected

    // --- 3. Perspective / MyRank Logic ---
    [<Theory>]
    [<MemberData(nameof(PerspectiveData))>]
    let ``MyRank respects player perspective correctly`` (rank: Rank, player: Player, expected: Rank) =
        Player.MyRank rank player |> should equal expected

    // --- 4. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<PlayerGenerator> |])>]
    let ``PlayerOther is its own inverse`` (p: Player) =
        // Applying PlayerOther twice should return the original player
        p |> Player.PlayerOther |> Player.PlayerOther = p

    [<Property(Arbitrary = [| typeof<PlayerGenerator> |])>]
    let ``PlayerOther never returns the same player`` (p: Player) =
        p |> Player.PlayerOther <> p

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``MyRank for White is always equal to the input rank`` (r: Rank) =
        Player.MyRank r Player.White = r

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``MyRank for Black and White together sums to 7`` (r: Rank) =
        // Rank indices are 0-7. Rank 1 (0) for White + Rank 1 (7) for Black = 7.
        int (Player.MyRank r Player.White) + int (Player.MyRank r Player.Black) = 7