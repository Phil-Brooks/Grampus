namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module UciParser =

    // --- 1. parseScore Tests ---

    [<Fact>]
    let ``parseScore identifies centipawn scores correctly`` () =
        let parts = "info depth 10 score cp 45 nodes 100".Split(' ')
        UciParser.parseScore parts |> should equal (Centipawns 45)

    [<Fact>]
    let ``parseScore identifies mate scores correctly`` () =
        let parts = "info score mate -3 nodes 10".Split(' ')
        UciParser.parseScore parts |> should equal (MateIn -3)

    [<Fact>]
    let ``parseScore returns Unknown for missing score data`` () =
        let parts = "info depth 10 nodes 100".Split(' ')
        UciParser.parseScore parts |> should equal Unknown


    // --- 2. parseInfo Tests ---

    [<Fact>]
    let ``parseInfo returns None for non-info lines`` () =
        UciParser.parseInfo "readyok" |> should equal None
        UciParser.parseInfo "option name Hash type spin default 1" |> should equal None

    [<Fact>]
    let ``parseInfo returns None if PV is missing`` () =
        // Engines sometimes send info lines without PV (e.g., current move)
        UciParser.parseInfo "info depth 10 nodes 500 score cp 10" |> should equal None

    [<Fact>]
    let ``parseInfo correctly parses a full UCI info string`` () =
        let input = "info depth 12 nodes 123456 nps 1000 score cp 67 pv e2e4 e7e5 g1f3"
        let result = UciParser.parseInfo input
        
        result.IsSome |> should be True
        let analysis = result.Value
        analysis.Depth |> should equal 12
        analysis.Nodes |> should equal 123456L
        analysis.Score |> should equal (Centipawns 67)
        analysis.Pv |> should equal ["e2e4"; "e7e5"; "g1f3"]

    [<Fact>]
    let ``parseInfo handles mate scores in full string`` () =
        let input = "info depth 5 nodes 500 score mate 2 pv f2f3 e7e5 g2g4 d8h4"
        let result = UciParser.parseInfo input
        
        result.Value.Score |> should equal (MateIn 2)
        result.Value.Pv |> should equal ["f2f3"; "e7e5"; "g2g4"; "d8h4"]

    [<Fact>]
    let ``parseInfo uses default values when fields are missing`` () =
        // Missing nodes and depth
        let input = "info score cp 0 pv d2d4"
        let result = UciParser.parseInfo input
        
        result.Value.Depth |> should equal 0
        result.Value.Nodes |> should equal 0L
        result.Value.Pv |> should equal ["d2d4"]