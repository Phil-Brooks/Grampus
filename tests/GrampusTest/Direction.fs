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
            Dirn.DirN; Dirn.DirE; Dirn.DirS; Dirn.DirW
            Dirn.DirNE; Dirn.DirSE; Dirn.DirSW; Dirn.DirNW
            Dirn.DirNNE; Dirn.DirEEN; Dirn.DirEES; Dirn.DirSSE
            Dirn.DirSSW; Dirn.DirWWS; Dirn.DirWWN; Dirn.DirNNW 
        ] |> Arb.fromGen

module Direction =

    // --- 1. Constant Verification ---
    [<Fact>]
    let ``Direction arrays have correct counts`` () =
        Direction.AllDirectionsKnight.Length |> should equal 8
        Direction.AllDirectionsRook.Length |> should equal 4
        Direction.AllDirectionsBishop.Length |> should equal 4
        Direction.AllDirectionsQueen.Length |> should equal 8

    [<Theory>]
    [<InlineData(Player.White, Dirn.DirN)>]
    [<InlineData(Player.Black, Dirn.DirS)>]
    let ``MyNorth returns correct direction for player`` (player: Player, expected: Dirn) =
        Direction.MyNorth player |> should equal expected

    // --- 2. Specific Opposite Checks ---
    [<Theory>]
    [<InlineData(Dirn.DirN, Dirn.DirS)>]
    [<InlineData(Dirn.DirE, Dirn.DirW)>]
    [<InlineData(Dirn.DirNE, Dirn.DirSW)>]
    [<InlineData(Dirn.DirNW, Dirn.DirSE)>]
    let ``Opposite returns correct hardcoded mapping`` (input: Dirn, expected: Dirn) =
        Direction.Opposite input |> should equal expected

    // --- 3. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<DirectionGenerator> |])>]
    let ``Opposite is reversible`` (d: Dirn) =
        // Opposite(Opposite(d)) should be d
        Direction.Opposite (Direction.Opposite d) = d

    [<Property(Arbitrary = [| typeof<DirectionGenerator> |])>]
    let ``Opposite sum is zero`` (d: Dirn) =
        // The integer values of opposite directions should sum to 0
        int d + int (Direction.Opposite d) = 0

    [<Property(Arbitrary = [| typeof<DirectionGenerator> |])>]
    let ``Knight moves are never horizontal, vertical, or perfectly diagonal`` (d: Dirn) =
        // Knight moves are special (15, 17, 6, 10). 
        // This test ensures no knight move was accidentally assigned a sliding piece value.
        let isKnightMove = Direction.AllDirectionsKnight |> Array.contains d
        if isKnightMove then
            let absVal = abs (int d)
            // Sliding values are 1, 7, 8, 9. Knight values are NOT these.
            [1; 7; 8; 9] |> List.contains absVal |> should be False
        true