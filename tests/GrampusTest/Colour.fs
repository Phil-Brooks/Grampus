namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

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

