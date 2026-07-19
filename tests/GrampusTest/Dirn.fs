namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module Direction =

    // --- 1. Constant Verification ---
    [<Fact>]
    let ``Direction arrays have correct counts`` () =
        Dirn.AllDirectionsKnight.Length |> should equal 8
        Dirn.AllDirectionsRook.Length |> should equal 4
        Dirn.AllDirectionsBishop.Length |> should equal 4
        Dirn.AllDirectionsQueen.Length |> should equal 8

    [<Theory>]
    [<InlineData(0, Dirn.N)>]
    [<InlineData(1, Dirn.S)>]
    let ``MyNorth returns correct direction for player`` (player: int, expected: int) =
        Dirn.MyNorth player |> should equal expected

    // --- 2. Specific Opposite Checks ---
    [<Theory>]
    [<InlineData(Dirn.N, Dirn.S)>]
    [<InlineData(Dirn.E, Dirn.W)>]
    [<InlineData(Dirn.NE, Dirn.SW)>]
    [<InlineData(Dirn.NW, Dirn.SE)>]
    let ``Opposite returns correct hardcoded mapping`` (input: int, expected: int) =
        Dirn.Opposite input |> should equal expected

