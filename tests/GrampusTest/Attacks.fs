namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

module Attacks =

    // --- 1. Test Data Definitions ---

    let KnightTestData : obj array seq =
        seq {
            yield [| D4; 8 |] // Center
            yield [| A1; 2 |] // Corner
            yield [| B1; 3 |] // Edge
        }

    let KingTestData : obj array seq =
        seq {
            yield [| D4; 8 |] // Center
            yield [| A1; 3 |] // Corner
            yield [| A2; 5 |] // Edge
        }

    // --- 2. Leap Piece Tests ---

    [<Theory>]
    [<MemberData(nameof(KnightTestData))>]
    let ``KnightAttacks returns correct count for positions`` (sq: int, expectedCount: int) =
        let attacks = Attacks.KnightAttacks sq
        Bitboard.bitCount attacks |> should equal expectedCount

    [<Theory>]
    [<MemberData(nameof(KingTestData))>]
    let ``KingAttacks returns correct count for positions`` (sq: int, expectedCount: int) =
        let attacks = Attacks.KingAttacks sq
        Bitboard.bitCount attacks |> should equal expectedCount

    // --- 3. Pawns (These were [Fact] so they don't need changing) ---

    [<Fact>]
    let ``White PawnAttacks wrap-around check`` () =
        let attacks = Attacks.PawnAttacks A2 0
        Bitboard.containsPos B3 attacks |> should be True
        Bitboard.containsPos H3 attacks |> should be False
        Bitboard.bitCount attacks |> should equal 1

    // --- 4. Magic Consistency (Property tests don't use InlineData, so they work as-is) ---

    [<Property(Arbitrary = [| typeof<BitboardGenerator>; typeof<ChessDimGenerator> |])>]
    let ``Magic RookAttacks matches functional RookAttacksCalc`` (sq: int) (blockers: Bitboard) =
        if sq >= 0 && sq < 64 then
            let magicResult = Attacks.RookAttacks sq blockers
            let functionalResult = Attacks.RookAttacksCalc sq blockers
            magicResult = functionalResult
        else true

    [<Property(Arbitrary = [| typeof<BitboardGenerator>; typeof<ChessDimGenerator> |])>]
    let ``Magic BishopAttacks matches functional BishopAttacksCalc`` (sq: int) (blockers: Bitboard) =
        if sq >= 0 && sq < 64 then
            let magicResult = Attacks.BishopAttacks sq blockers
            let functionalResult = Attacks.BishopAttacksCalc sq blockers
            magicResult = functionalResult
        else true