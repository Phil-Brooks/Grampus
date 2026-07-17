namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module pMove =

    // --- 1. Parsing Tests (SAN to pMove Record) ---
    // 1. Define the test cases in a static sequence
    // Note: We box them into 'obj array' so xUnit can consume them
    let ParseTestData : obj array seq =
        seq {
            yield [| "e4"; PieceType.Pawn; E4; pMove.Simple |]
            yield [| "Nf3"; PieceType.Knight; F3; pMove.Simple |]
            yield [| "Bxe5"; PieceType.Bishop; E5; pMove.Capture |]
            yield [| "O-O"; PieceType.King; OUTOFBOUNDS; pMove.CastleKingSide |]
            yield [| "O-O-O"; PieceType.King; OUTOFBOUNDS; pMove.CastleQueenSide |]
        }

    // 2. Reference the data using MemberData
    [<Theory>]
    [<MemberData(nameof(ParseTestData))>]
    let ``Parse correctly identifies basic move components`` (san: string, expPiece: int, expTarget: int, expType: int) =
        let pm = pMove.Parse san
        pm.Piece |> should equal (Some expPiece)
        pm.TargetSquare |> should equal expTarget
        pm.Mtype |> should equal expType

    //[<Theory>]
    //[<InlineData("Qh5+", true, false)>]
    //[<InlineData("Rd8#", false, true)>]
    //[<InlineData("Bb5++", false, true)>] // Some notations use ++ for double check, your code handles + as check
    //let ``Parse detects check and checkmate symbols`` (san: string, expCheck: bool, expMate: bool) =
    //    let pm = pMove.Parse san
    //    pm.IsCheck |> should equal expCheck
    //    pm.IsCheckMate |> should equal expMate

    [<Theory>]
    [<InlineData("Ndf3", 'd')>]
    [<InlineData("R1a5", '1')>]
    let ``Parse identifies origin ambiguity`` (san: string, expectedChar: char) =
        let pm = pMove.Parse san
        if System.Char.IsDigit(expectedChar) then
            pm.OriginRank |> should equal (Some (Rank.fromChar expectedChar))
        else
            pm.OriginFile |> should equal (Some (File.fromChar expectedChar))

    [<Fact>]
    let ``Parse handles pawn promotions`` () =
        let pm = pMove.Parse "a8=Q"
        pm.Piece |> should equal (Some PieceType.Pawn)
        pm.PromotedPiece |> should equal (Some PieceType.Queen)
        pm.TargetSquare |> should equal A8

