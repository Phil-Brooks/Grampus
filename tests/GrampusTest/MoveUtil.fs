namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module MoveUtil =

    // --- 1. Integration Test Data ---
    let SanIntegrationData : obj array seq =
        seq {
            // [| FEN string; SAN input; Expected From; Expected To |]
            
            // Standard opening
            yield [| FEN.StartStr; "e4"; E2; E4 |]
            yield [| FEN.StartStr; "Nf3"; G1; F3 |]
            
            // Simple Capture
            // 1. e4 d5 2. exd5
            yield [| "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2"; 
                     "exd5"; E4; D5 |]
            
            // Castling
            yield [| "rnbqk2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1"; 
                     "O-O"; E1; G1 |]
            
            // Ambiguity Resolution (Two Knights can reach d2)
            yield [| "rnbqkbnr/pppppppp/8/8/8/5N2/PPP1PPPP/RNBQKB1R w KQkq - 0 1"; 
                     "Nbd2"; B1; D2 |]
        }

    // --- 2. Integration Tests ---

    [<Theory>]
    [<MemberData(nameof(SanIntegrationData))>]
    let ``fromSAN correctly resolves string to bit-encoded move`` 
        (fen: string, san: string, expFrom: Square, expTo: Square) =
        
        let bd = FEN.Parse fen |> Board.FromFEN
        let actualMove = MoveUtil.fromSAN bd san
        
        Move.From actualMove |> should equal expFrom
        Move.To actualMove |> should equal expTo

    [<Fact>]
    let ``fromSAN handles pawn promotions correctly`` () =
        let fen = "8/P7/k7/8/8/8/8/4K3 w - - 0 1"
        let bd = FEN.Parse fen |> Board.FromFEN
        
        let actualMove = MoveUtil.fromSAN bd "a8=Q"
        
        Move.IsPromotion actualMove |> should be True
        Move.PromoteType actualMove |> should equal PieceType.Queen

    [<Fact>]
    let ``fromSAN throws on illegal move string`` () =
        // King cannot move to E3 in start position
        let bd = Board.Start
        (fun () -> MoveUtil.fromSAN bd "Ke3" |> ignore) 
        |> should throw typeof<System.Exception>

    [<Fact>]
    let ``fromSAN throws on malformed move string`` () =
        let bd = Board.Start
        (fun () -> MoveUtil.fromSAN bd "NotAMove" |> ignore) 
        |> should throw typeof<System.Exception>