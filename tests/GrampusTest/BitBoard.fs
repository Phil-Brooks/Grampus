namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus
open System.Numerics

type BitboardGenerator =
    static member Bitboard() =
        // FsCheck 3 uses ArbMap to find generators for types
        ArbMap.defaults 
        |> ArbMap.generate<uint64>
        |> FsCheck.FSharp.Gen.map (LanguagePrimitives.EnumOfValue<uint64, Bitboard>)
        |> Arb.fromGen
        
module Bitboard =

    // --- 1. Basic Counting and Conversion ---

    [<Property(Arbitrary = [| typeof<BitboardGenerator> |])>]
    let ``bitCount matches toSquares length`` (bb: Bitboard) =
        Bitboard.bitCount bb = (Bitboard.toSquares bb).Length

    [<Property(Arbitrary = [| typeof<BitboardGenerator> |])>]
    let ``popFirst reduces bitCount by exactly one`` (bb: Bitboard) =
        if bb = Bitboard.Empty then
            let sq, remaining = Bitboard.popFirst bb
            sq = OUTOFBOUNDS && remaining = Bitboard.Empty
        else
            let originalCount = Bitboard.bitCount bb
            let _, remaining = Bitboard.popFirst bb
            Bitboard.bitCount remaining = originalCount - 1

    // --- 2. Shifting and Board Edges (Clipping) ---

    [<Fact>]
    let ``shiftN clips bits on Rank 8`` () =
        // Shifting Rank 8 North should result in an Empty board
        Bitboard.shiftN Bitboard.Rank8 |> should equal Bitboard.Empty

    [<Fact>]
    let ``shiftS clips bits on Rank 1`` () =
        Bitboard.shiftS Bitboard.Rank1 |> should equal Bitboard.Empty

    [<Fact>]
    let ``shiftE clips bits on File H`` () =
        Bitboard.shiftE Bitboard.FileH |> should equal Bitboard.Empty

    [<Fact>]
    let ``shiftW clips bits on File A`` () =
        Bitboard.shiftW Bitboard.FileA |> should equal Bitboard.Empty

    // --- 3. Directional Consistency ---

    [<Property(Arbitrary = [| typeof<BitboardGenerator> |])>]
    let ``Shifting North then South is reversible for non-Rank8 bits`` (bb: Bitboard) =
        let input = bb &&& ~~~Bitboard.Rank8
        let result = input |> Bitboard.shiftN |> Bitboard.shiftS
        result = input

    [<Property(Arbitrary = [| typeof<BitboardGenerator> |])>]
    let ``Shifting West then East is reversible for non-FileA bits`` (bb: Bitboard) =
        let input = bb &&& ~~~Bitboard.FileA
        // Note: Based on your code, West is <<< 1 and East is >>> 1
        let result = input |> Bitboard.shiftW |> Bitboard.shiftE
        result = input

    // --- 4. Flood (Ray casting) ---

    [<Fact>]
    let ``flood North from A1 creates the whole A File`` () =
        let start = LanguagePrimitives.EnumOfValue<uint64, Bitboard>(1UL <<< int A1)
        let result = Bitboard.flood Dirn.DirN start
        result |> should equal Bitboard.FileA

    // --- 5. Containment ---

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``containsPos is true for a bitboard created from that square`` (sq: Square) =
        if sq >= 0s && sq < 64s then
            let bb = LanguagePrimitives.EnumOfValue<uint64, Bitboard>(1UL <<< int sq)
            Bitboard.containsPos sq bb
        else true