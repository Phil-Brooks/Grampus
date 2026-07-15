namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module FEN =

    // --- 1. Test Data for Theories ---
    // Using MemberData because FEN strings are too complex for InlineData
    let FenTestData : obj array seq =
        seq {
            // 1. Starting Position
            yield [| FEN.StartStr; 0; true; true; true; true; OUTOFBOUNDS; 0; 1 |]
            // 2. Mid-game with specific rights and EP
            // Position after 1. e4 c5 2. Nf3 d6 3. d4 cxd4
            yield [| "rnbqkbnr/pp2pppp/3p4/8/3pP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 0 4"; 
                     0; true; true; true; true; OUTOFBOUNDS; 0; 4 |]
            // 3. No castling rights, Black to move
            yield [| "4k3/8/8/8/8/8/4P3/4K3 b - - 5 10"; 
                     1; false; false; false; false; OUTOFBOUNDS; 5; 10 |]
            // 4. En Passant square active
            yield [| "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"; 
                     1; true; true; true; true; E3; 0; 1 |]
        }

    // --- 2. Functional Tests ---

    [<Theory>]
    [<MemberData(nameof(FenTestData))>]
    let ``Parse correctly extracts metadata from FEN string`` 
        (fenStr: string, turn, cwS, cwL, cbS, cbL, ep, fifty, full) =
        
        let result = FEN.Parse fenStr
        
        result.Whosturn |> should equal turn
        result.CastleWS |> should equal cwS
        result.CastleWL |> should equal cwL
        result.CastleBS |> should equal cbS
        result.CastleBL |> should equal cbL
        result.Enpassant |> should equal ep
        result.Fiftymove |> should equal fifty
        result.Fullmove |> should equal full

    [<Fact>]
    let ``Parse correctly places pieces for the starting position`` () =
        let result = FEN.Parse FEN.StartStr
        
        // Check corners
        result.Pieceat.[int A1] |> should equal Piece.WRook
        result.Pieceat.[int H1] |> should equal Piece.WRook
        result.Pieceat.[int A8] |> should equal Piece.BRook
        result.Pieceat.[int H8] |> should equal Piece.BRook
        
        // Check Kings
        result.Pieceat.[int E1] |> should equal Piece.WKing
        result.Pieceat.[int E8] |> should equal Piece.BKing
        
        // Check an empty square in the middle
        result.Pieceat.[int D4] |> should equal Piece.EMPTY

    [<Fact>]
    let ``Parse handles numeric empty square notation (e.g., 8)`` () =
        // FEN with a completely empty Rank 4
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let result = FEN.Parse fen
        
        [A4..H4] |> List.iter (fun sq -> 
            result.Pieceat.[int sq] |> should equal Piece.EMPTY)

    [<Fact>]
    let ``Parse throws exception for invalid FEN`` () =
        let invalidFen = "not a real fen string"
        (fun () -> FEN.Parse invalidFen |> ignore) |> should throw typeof<System.Exception>

    // --- 3. Integration Property ---
    // (If you eventually add a FEN.ToString function, add a Round-Trip property here)