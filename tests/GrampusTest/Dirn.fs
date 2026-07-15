namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

type DirectionGenerator =
    static member Dirn() =
        FsCheck.FSharp.Gen.elements [ 
            Dirn.N; Dirn.E; Dirn.S; Dirn.W
            Dirn.NE; Dirn.SE; Dirn.SW; Dirn.NW
            Dirn.NNE; Dirn.EEN; Dirn.EES; Dirn.SSE
            Dirn.SSW; Dirn.WWS; Dirn.WWN; Dirn.NNW 
        ] |> Arb.fromGen

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

    // --- 3. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<DirectionGenerator> |])>]
    let ``Opposite is reversible`` (d: int) =
        // Opposite(Opposite(d)) should be d
        Dirn.Opposite (Dirn.Opposite d) = d

    [<Property(Arbitrary = [| typeof<DirectionGenerator> |])>]
    let ``Opposite sum is zero`` (d: int) =
        // The integer values of opposite directions should sum to 0
        int d + int (Dirn.Opposite d) = 0

    [<Property(Arbitrary = [| typeof<DirectionGenerator> |])>]
    let ``Knight moves are never horizontal, vertical, or perfectly diagonal`` (d: int) =
        // Knight moves are special (15, 17, 6, 10). 
        // This test ensures no knight move was accidentally assigned a sliding piece value.
        let isKnightMove = Dirn.AllDirectionsKnight |> Array.contains d
        if isKnightMove then
            let absVal = abs (int d)
            // Sliding values are 1, 7, 8, 9. Knight values are NOT these.
            [1; 7; 8; 9] |> List.contains absVal |> should be False
        true