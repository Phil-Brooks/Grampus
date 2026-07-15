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
        |> Arb.fromGen
        
module Bitboard =

    // --- 1. Basic Counting and Conversion ---

    [<Property(Arbitrary = [| typeof<BitboardGenerator> |])>]
    let ``bitCount matches toSquares length`` (bb: uint64) =
        Bitboard.bitCount bb = (Bitboard.toSquares bb).Length

    [<Property(Arbitrary = [| typeof<BitboardGenerator> |])>]
    let ``popFirst reduces bitCount by exactly one`` (bb: uint64) =
        if bb = 0UL then
            let sq, remaining = Bitboard.popFirst bb
            sq = OUTOFBOUNDS && remaining = 0UL
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
    let ``Shifting North then South is reversible for non-Rank8 bits`` (bb: uint64) =
        let input = bb &&& ~~~Bitboard.Rank8
        let result = input |> Bitboard.shiftN |> Bitboard.shiftS
        result = input

    [<Property(Arbitrary = [| typeof<BitboardGenerator> |])>]
    let ``Shifting West then East is reversible for non-FileA bits`` (bb: uint64) =
        let input = bb &&& ~~~Bitboard.FileA
        // Note: Based on your code, West is <<< 1 and East is >>> 1
        let result = input |> Bitboard.shiftW |> Bitboard.shiftE
        result = input

    // --- 4. Flood (Ray casting) ---

    [<Fact>]
    let ``flood North from A1 creates the whole A File`` () =
        let start = 1UL <<< int A1
        let result = Bitboard.flood Dirn.N start
        result |> should equal Bitboard.FileA

    // --- 5. Containment ---

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``containsPos is true for a bitboard created from that square`` (sq: int) =
        if sq >= 0 && sq < 64 then
            let bb = 1UL <<< sq
            Bitboard.containsPos sq bb
        else true