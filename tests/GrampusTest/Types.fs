namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

// Generators to help FsCheck test valid Files, Ranks, and Squares
type ChessDimGenerator =
    static member File() = Gen.elements [ 0s .. 7s ] |> Arb.fromGen
    static member Rank() = Gen.elements [ 0s .. 7s ] |> Arb.fromGen
    static member Square() = Gen.elements [ 0s .. 63s ] |> Arb.fromGen

module Types =

    // --- 1. Constant Verification ---
    [<Fact>]
    let ``Square constants have correct values``() =
        A1 |> should equal 0
        H1 |> should equal 7
        A8 |> should equal 56
        H8 |> should equal 63

    [<Fact>]
    let ``OUTOFBOUNDS is set to 64``() =
        OUTOFBOUNDS |> should equal 64

    // --- 2. Logic Verification (Sq function) ---
    [<Theory>]
    [<InlineData(0, 0, 0)>]   // A1
    [<InlineData(7, 0, 7)>]   // H1
    [<InlineData(0, 7, 56)>]  // A8
    [<InlineData(7, 7, 63)>]  // H8
    [<InlineData(4, 3, 28)>]  // E4
    let ``Sq function calculates correct index``(f, r, expected) =
        Sq(f, r) |> should equal expected

    // --- 3. Bitboard Constant Verification ---

    // --- 4. Board Representation ---
    [<Fact>]
    let ``BrdEMP string representation is correct``() =
        let str = Board.EMPTY.ToString()
        // Should be 64 dots followed by " w"
        str |> should equal ((String.replicate 64 ".") + " w")

    // --- 5. Property Based Testing (FsCheck) ---
    
    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Any Sq(f, r) is always within 0-63`` (f: int) (r: int) =
        let s = Sq(f, r)
        s >= 0 && s <= 63

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Sq function is reversible (Modulo 8)`` (f: int) (r: int) =
        let s = Sq(f, r)
        let recoveredFile = s % 8
        let recoveredRank = s / 8
        recoveredFile = f && recoveredRank = r
