namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

// Custom generator for Pieces to ensure FsCheck only tests valid Enum values
type PieceGenerator =
    static member Piece() =
        Gen.elements [ 
            Piece.WPawn; Piece.WKnight; Piece.WBishop; Piece.WRook; Piece.WQueen; Piece.WKing;
            Piece.BPawn; Piece.BKnight; Piece.BBishop; Piece.BRook; Piece.BQueen; Piece.BKing;
            Piece.EMPTY 
        ] |> Arb.fromGen

module Piece =
    
    // --- 1. Exhaustive Unit Tests (Theory) ---
    
    [<Theory>]
    [<InlineData('P', Piece.WPawn)>]
    [<InlineData('k', Piece.BKing)>]
    [<InlineData('n', Piece.BKnight)>]
    [<InlineData('.', Piece.EMPTY)>]
    let ``Parse returns correct Piece for valid char``(input: char, expected: int) =
        Piece.Parse input |> should equal expected

    [<Fact>]
    let ``Parse throws exception for invalid char``() =
        (fun () -> Piece.Parse 'Z' |> ignore) |> should throw typeof<System.Exception>

    [<Theory>]
    [<InlineData(Piece.WPawn, "P")>]
    [<InlineData(Piece.BQueen, "q")>]
    [<InlineData(Piece.EMPTY, ".")>]
    let ``ToStr returns correct string for Piece``(input, expected) =
        Piece.ToStr input |> should equal expected


    // --- 2. Property Based Testing (FsCheck) ---

    [<Property(Arbitrary = [| typeof<PieceGenerator> |])>]
    let ``Piece Round-trip: Parse(ToStr(p)) equals p`` (p: int) =
        let s = Piece.ToStr p
        Piece.Parse (s.[0]) = p

    [<Property(Arbitrary = [| typeof<PieceGenerator> |])>]
    let ``PieceToPlayer logic is consistent`` (p: int) =
        match Piece.ToColour p with
        | None -> p = Piece.EMPTY
        | Some player ->
            match player with
            | 0 -> (int p > 0 && int p < 8)
            | 1 -> (int p > 8)
            | _ -> false // Handles values like enum<Player>(2)
    
    [<Property(Arbitrary = [| typeof<PieceGenerator> |])>]
    let ``ToPcType ignores color bit`` (p: int) =
        if p = Piece.EMPTY then 
            Piece.ToPcType p = PcType.EMPTY
        else
            let pt = Piece.ToPcType p
            // Property: PcType should be between 1 (Pawn) and 6 (King)
            int pt >= 1 && int pt <= 6

    [<Property>]
    let ``Any valid char from Parse must result in a valid PieceToPlayer or EMPTY`` (c: char) =
        let validChars = "PNBRQKpnbrqk."
        if validChars.Contains(c) then
            let p = Piece.Parse c
            p = Piece.EMPTY || (Piece.ToColour p).IsSome
        else
            true // FsCheck will generate random chars; we only care about valid ones here