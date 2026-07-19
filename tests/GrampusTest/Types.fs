namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

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
        SQ(f, r) |> should equal expected

    // --- 3. Bitboard Constant Verification ---

    // --- 4. Board Representation ---
    [<Fact>]
    let ``BrdEMP string representation is correct``() =
        let str = Board.EMP.ToString()
        // Should be 64 dots followed by " w"
        str |> should equal ((String.replicate 64 ".") + " w")

