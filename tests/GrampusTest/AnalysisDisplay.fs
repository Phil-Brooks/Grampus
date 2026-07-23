namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module AnalysisDisplay =

    [<Fact>]
    let ``formatScore flips sign correctly for Black perspective`` () =
        let score = Centipawns 50 // Engine says +0.50 for side to move
        
        // If it's White's turn (0), stays +0.50
        AnalysisDisplay.formatScore 0 score |> should equal "+0.50"
        
        // If it's Black's turn (1), engine's +0.50 means Black is winning (-0.50)
        AnalysisDisplay.formatScore 1 score |> should equal "-0.50"

    [<Fact>]
    let ``formatNodes simplifies large numbers`` () =
        AnalysisDisplay.formatNodes 1200L |> should equal "1.2k"
        AnalysisDisplay.formatNodes 2500000L |> should equal "2.5M"
        AnalysisDisplay.formatNodes 500L |> should equal "500"

    [<Fact>]
    let ``getSanPv generates full numbered sequence from FEN context`` () =
        // Set up a board where it is White's turn at move 1
        let bd = Board.Start 
        let uciMoves = ["e2e4"; "e7e5"; "g1f3"]
        
        let result = AnalysisDisplay.getSanPv bd uciMoves
        
        // Expected: 1. e4 e5 2. Nf3
        result |> should equal "1. e4 e5 2. ♘f3"

    [<Fact>]
    let ``getSanPv handles Black to move starting sequence`` () =
        // Set up board after 1. e4
        let bd = Board.MoveApply { From=12; To=28; Pc=WPAWN; CapPc=0; Prom=0 } Board.Start
        let uciMoves = ["e7e5"; "g1f3"]
        
        let result = AnalysisDisplay.getSanPv bd uciMoves
        
        // Expected: e5 2. Nf3 (standard notation when starting on Black's turn)
        result |> should equal "e5 2. ♘f3"