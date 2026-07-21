namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module FEN =
    // Helper to create a blank board with default values
    let emptyBoard = {
        PieceAt = Array.create 64 EMPTY
        WtKingPos = E1; BkKingPos = E8
        WhosTurn = WHITE
        CastleRts = { WK = false; WQ = false; BK = false; BQ = false }
        EnPassant = OUTOFBOUNDS
        Fiftymove = 0; Fullmove = 1
    }

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
    let ``ToBrd correctly extracts metadata from FEN string`` 
        (fenStr: string, turn, cwK, cwQ, cbK, cbQ, ep, fifty, full) =
        
        let result = FEN.ToBrd fenStr
        
        result.WhosTurn |> should equal turn
        result.CastleRts.WK |> should equal cwK
        result.CastleRts.WQ |> should equal cwQ
        result.CastleRts.BK |> should equal cbK
        result.CastleRts.BQ |> should equal cbQ
        result.EnPassant |> should equal ep
        result.Fiftymove |> should equal fifty
        result.Fullmove |> should equal full

    [<Fact>]
    let ``ToBrd correctly places pieces for the starting position`` () =
        let result = FEN.ToBrd FEN.StartStr
        
        // Check corners
        result.PieceAt.[int A1] |> should equal WROOK
        result.PieceAt.[int H1] |> should equal WROOK
        result.PieceAt.[int A8] |> should equal BROOK
        result.PieceAt.[int H8] |> should equal BROOK
        
        // Check Kings
        result.PieceAt.[int E1] |> should equal WKING
        result.PieceAt.[int E8] |> should equal BKING
        
        // Check an empty square in the middle
        result.PieceAt.[int D4] |> should equal EMPTY

    [<Fact>]
    let ``ToBrd handles numeric empty square notation (e.g., 8)`` () =
        // FEN with a completely empty Rank 4
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let result = FEN.ToBrd fen
        
        [A4..H4] |> List.iter (fun sq -> 
            result.PieceAt.[int sq] |> should equal EMPTY)

    [<Fact>]
    let ``ToBrd throws exception for invalid FEN`` () =
        let invalidFen = "not a real fen string"
        (fun () -> FEN.ToBrd invalidFen |> ignore) |> should throw typeof<System.Exception>

    [<Fact>]
    let ``Starting position generates correct FEN string`` () =
        // Note: You'll need a function/value that returns the initial board state
        let board = Board.Start 
        let fen = FEN.FromBrd board
        fen |> should equal "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    [<Fact>]
    let ``Board after 1. e4 generates correct FEN`` () =
        // Manually constructing the state for 1. e4
        let pAt = Array.copy Board.Start.PieceAt
        pAt.[E2] <- EMPTY
        pAt.[E4] <- WPAWN
        
        let board = { 
            Board.Start with 
                PieceAt = pAt
                WhosTurn = BLACK
                EnPassant = E3 // Target square for en passant
                Fullmove = 1 
        }
        
        let fen = FEN.FromBrd board
        // Assuming Square.ToStr converts E3 to "e3"
        fen |> should equal "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

    [<Theory>]
    [<InlineData(WHITE, "w")>]
    [<InlineData(BLACK, "b")>]
    let ``WhosTurn is correctly mapped in FEN`` (turn, expected) =
        let board = { emptyBoard with WhosTurn = turn }
        let fen = FEN.FromBrd board
        let parts = fen.Split(' ')
        parts.[1] |> should equal expected

    [<Fact>]
    let ``Castling rights are correctly formatted`` () =
        let board = { emptyBoard with CastleRts = { WK=true; WQ=false; BK=true; BQ=false } }
        let fen = FEN.FromBrd board
        let parts = fen.Split(' ')
        parts.[2] |> should equal "Kk"

    [<Theory>]
    // Standard starting position
    [<InlineData("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")>]
    // After 1. e4
    [<InlineData("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")>]
    // Partial castling rights (only White King-side and Black Queen-side)
    [<InlineData("r3k2r/8/8/8/8/8/8/R3K2R w Kq - 5 12")>]
    // No castling rights
    [<InlineData("r3k2r/8/8/8/8/8/8/R3K2R w - - 0 1")>]
    // Endgame with pawns and kings only
    [<InlineData("8/8/4k3/8/8/4K3/4P3/8 w - - 10 42")>]
    // Nimzo-Indian / Midgame complex board
    [<InlineData("r1bq1rk1/ppp2ppp/1pn1pn2/3p4/2PP4/2N1PN2/PP3PPP/R1BQKB1R w KQ - 2 7")>]
    let ``FEN string round-trip: FEN -> ToBrd -> FromBrd yields identical FEN`` (originalFen: string) =
        let board = FEN.ToBrd originalFen
        let generatedFen = FEN.FromBrd board
        generatedFen |> should equal originalFen

    [<Fact>]
    let ``Board round-trip: Brd -> FromBrd -> ToBrd restores all Brd fields`` () =
        let originalBoard = Board.Start
        
        let fen = FEN.FromBrd originalBoard
        let restoredBoard = FEN.ToBrd fen

        // Test equality of core fields
        restoredBoard.WhosTurn |> should equal originalBoard.WhosTurn
        restoredBoard.CastleRts |> should equal originalBoard.CastleRts
        restoredBoard.EnPassant |> should equal originalBoard.EnPassant
        restoredBoard.Fiftymove |> should equal originalBoard.Fiftymove
        restoredBoard.Fullmove |> should equal originalBoard.Fullmove
        restoredBoard.PieceAt |> should equal originalBoard.PieceAt
        
        // Ensure king positions were correctly re-calculated during parsing
        restoredBoard.WtKingPos |> should equal originalBoard.WtKingPos
        restoredBoard.BkKingPos |> should equal originalBoard.BkKingPos
