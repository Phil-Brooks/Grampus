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
            yield [| "e4"; PieceType.Pawn; E4; MoveType.Simple |]
            yield [| "Nf3"; PieceType.Knight; F3; MoveType.Simple |]
            yield [| "Bxe5"; PieceType.Bishop; E5; MoveType.Capture |]
            yield [| "O-O"; PieceType.King; OUTOFBOUNDS; MoveType.CastleKingSide |]
            yield [| "O-O-O"; PieceType.King; OUTOFBOUNDS; MoveType.CastleQueenSide |]
        }

    // 2. Reference the data using MemberData
    [<Theory>]
    [<MemberData(nameof(ParseTestData))>]
    let ``Parse correctly identifies basic move components`` (san: string, expPiece: PieceType, expTarget: Square, expType: MoveType) =
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
            pm.OriginRank |> should equal (Some (Rank.Parse expectedChar))
        else
            pm.OriginFile |> should equal (Some (File.Parse expectedChar))

    [<Fact>]
    let ``Parse handles pawn promotions`` () =
        let pm = pMove.Parse "a8=Q"
        pm.Piece |> should equal (Some PieceType.Pawn)
        pm.PromotedPiece |> should equal (Some PieceType.Queen)
        pm.TargetSquare |> should equal A8

    // --- 2. ToMove Integration Tests (pMove + Board -> Move) ---

    [<Fact>]
    let ``ToMove resolves ambiguous knight moves using origin file`` () =
        // Setup: White Knights on d2 and f2, both can move to e4.
        // Move "Nde4"
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPNPNPP/RNBQKB1R w KQkq - 0 1"
        let bd = FEN.Parse fen |> Board.FromFEN
        // Verify both knights can reach E4
        MoveGenerate.KnightMovesTo E4 bd |> List.length |> should equal 2
        
        let pm = pMove.Parse "Nde4"
        let actualMove = pMove.ToMove bd pm
        
        Move.From actualMove |> should equal D2
        Move.To actualMove |> should equal E4

    [<Fact>]
    let ``ToMove identifies castling move correctly`` () =
        let bd = Board.Start
        // Normally you can't castle on move 1, so let's use a custom FEN
        let fen = "rnbqk2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1"
        let bd = FEN.Parse fen |> Board.FromFEN
        
        let pm = pMove.Parse "O-O"
        let actualMove = pMove.ToMove bd pm
        
        Move.IsCastle actualMove |> should be True
        Move.To actualMove |> should equal G1

    [<Fact>]
    let ``ToMove identifies en passant correctly`` () =
        // White just played e4, Black pawn on d4 can capture exd3 e.p.
        let fen = "rnbqkbnr/ppp1pppp/8/8/3pP3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        let bd = FEN.Parse fen |> Board.FromFEN
        
        let pm = pMove.Parse "dxe3"
        let actualMove = pMove.ToMove bd pm
        
        Move.IsEnPassant actualMove |> should be True
        Move.To actualMove |> should equal E3