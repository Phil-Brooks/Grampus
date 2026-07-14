namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

type PieceTypeGenerator =
    static member PieceType() =
        Gen.elements [ 
            PieceType.Pawn; PieceType.Knight; PieceType.Bishop; 
            PieceType.Rook; PieceType.Queen; PieceType.King 
        ] |> Arb.fromGen


module PieceType =

    // --- 1. Parsing Tests ---

    [<Theory>]
    [<InlineData('P', PieceType.Pawn)>]
    [<InlineData('p', PieceType.Pawn)>] // Verify case-insensitivity
    [<InlineData('N', PieceType.Knight)>]
    [<InlineData('b', PieceType.Bishop)>]
    [<InlineData('R', PieceType.Rook)>]
    [<InlineData('Q', PieceType.Queen)>]
    [<InlineData('k', PieceType.King)>]
    let ``Parse returns correct PieceType regardless of case`` (c: char, expected: PieceType) =
        PieceType.Parse c |> should equal expected

    [<Fact>]
    let ``Parse throws for invalid characters`` () =
        (fun () -> PieceType.Parse 'Z' |> ignore) |> should throw typeof<System.Exception>

    // --- 2. ForPlayer (Bit-packing) Tests ---

    [<Theory>]
    [<InlineData(Player.White, PieceType.Pawn, Piece.WPawn)>]     // 1 | (0 << 3) = 1
    [<InlineData(Player.Black, PieceType.Pawn, Piece.BPawn)>]     // 1 | (1 << 3) = 9
    [<InlineData(Player.White, PieceType.King, Piece.WKing)>]     // 6 | (0 << 3) = 6
    [<InlineData(Player.Black, PieceType.King, Piece.BKing)>]     // 6 | (1 << 3) = 14
    [<InlineData(Player.Black, PieceType.Queen, Piece.BQueen)>]   // 5 | (1 << 3) = 13
    let ``ForPlayer correctly packs player and type into a Piece`` (player: Player, pt: PieceType, expected: Piece) =
        PieceType.ForPlayer player pt |> should equal expected

    // --- 3. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<PieceTypeGenerator> |])>]
    let ``Parse(c) matches Parse(toUpper c)`` (c: char) =
        let valid = "PNBRQKpnbrqk"
        if valid.Contains(c) then
            PieceType.Parse c = PieceType.Parse (System.Char.ToUpper c)
        else true

    [<Property(Arbitrary = [| typeof<PieceTypeGenerator>; typeof<PlayerGenerator> |])>]
    let ``ForPlayer results are reversible using Piece logic`` (player: Player) (pt: PieceType) =
        // Arrange: Create a piece
        let piece = PieceType.ForPlayer player pt
        
        // Assert: Using logic from our Piece module to verify the packing
        let recoveredType = Piece.ToPieceType piece
        let recoveredPlayer = Piece.PieceToPlayer piece
        
        recoveredType = pt && recoveredPlayer = Some player

    [<Property(Arbitrary = [| typeof<PieceTypeGenerator> |])>]
    let ``ForPlayer White is always equal to the integer value of PieceType`` (pt: PieceType) =
        let piece = PieceType.ForPlayer Player.White pt
        int piece = int pt