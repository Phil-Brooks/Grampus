namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

type PcTypeGenerator =
    static member PcType() =
        Gen.elements [ 
            PcType.Pawn; PcType.Knight; PcType.Bishop; 
            PcType.Rook; PcType.Queen; PcType.King 
        ] |> Arb.fromGen


module PcType =

    // --- 1. Parsing Tests ---

    [<Theory>]
    [<InlineData('P', PcType.Pawn)>]
    [<InlineData('p', PcType.Pawn)>] // Verify case-insensitivity
    [<InlineData('N', PcType.Knight)>]
    [<InlineData('b', PcType.Bishop)>]
    [<InlineData('R', PcType.Rook)>]
    [<InlineData('Q', PcType.Queen)>]
    [<InlineData('k', PcType.King)>]
    let ``Parse returns correct PcType regardless of case`` (c: char, expected: int) =
        PcType.fromChar c |> should equal expected

    [<Fact>]
    let ``Parse throws for invalid characters`` () =
        (fun () -> PcType.fromChar 'Z' |> ignore) |> should throw typeof<System.Exception>

    // --- 2. ForPlayer (Bit-packing) Tests ---

    [<Theory>]
    [<InlineData(0, PcType.Pawn, Piece.WPawn)>]     // 1 | (0 << 3) = 1
    [<InlineData(1, PcType.Pawn, Piece.BPawn)>]     // 1 | (1 << 3) = 9
    [<InlineData(0, PcType.King, Piece.WKing)>]     // 6 | (0 << 3) = 6
    [<InlineData(1, PcType.King, Piece.BKing)>]     // 6 | (1 << 3) = 14
    [<InlineData(1, PcType.Queen, Piece.BQueen)>]   // 5 | (1 << 3) = 13
    let ``ForPlayer correctly packs colour and type into a Piece`` (colour: int, pt: int, expected: int) =
        PcType.Piece colour pt |> should equal expected

    // --- 3. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<PcTypeGenerator> |])>]
    let ``Parse(c) matches Parse(toUpper c)`` (c: char) =
        let valid = "PNBRQKpnbrqk"
        if valid.Contains(c) then
            PcType.fromChar c = PcType.fromChar (System.Char.ToUpper c)
        else true

    [<Property(Arbitrary = [| typeof<PcTypeGenerator>; typeof<PlayerGenerator> |])>]
    let ``ForPlayer results are reversible using Piece logic`` (colour: int) (pt: int) =
        // Arrange: Create a piece
        let piece = PcType.Piece colour pt
        
        // Assert: Using logic from our Piece module to verify the packing
        let recoveredType = Piece.ToPcType piece
        let recoveredPlayer = Piece.ToColour piece
        
        recoveredType = pt && recoveredPlayer = Some colour

    [<Property(Arbitrary = [| typeof<PcTypeGenerator> |])>]
    let ``ForPlayer White is always equal to the integer value of PcType`` (pt: int) =
        let piece = PcType.Piece 0 pt
        int piece = int pt