namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module Engine =

    // --- 1. Testing Command Formatting (Requests) ---

    [<Theory>]
    [<InlineData("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")>]
    let ``formatRequest: SetPosition creates correct UCI string`` (fen: string) =
        let cmd = UciProtocol.formatRequest (SetPosition fen)
        cmd |> should equal (sprintf "position fen %s" fen)

    [<Fact>]
    let ``formatRequest: StartSearch uses correct millisecond value`` () =
        let cmd = UciProtocol.formatRequest (StartSearch 5000)
        cmd |> should equal "go movetime 5000"

    [<Fact>]
    let ``formatRequest: Stop and Quit return correct strings`` () =
        UciProtocol.formatRequest StopSearch |> should equal "stop"
        UciProtocol.formatRequest Quit |> should equal "quit"


    // --- 2. Testing Line Parsing (Responses) ---

    [<Fact>]
    let ``parseLine: readyok returns Ready message`` () =
        UciProtocol.parseLine "readyok" |> should equal (Some Ready)

    [<Fact>]
    let ``parseLine: bestmove extracts the move correctly`` () =
        UciProtocol.parseLine "bestmove e2e4" |> should equal (Some (BestMove "e2e4"))
        UciProtocol.parseLine "bestmove g1f3 ponder d7d5" |> should equal (Some (BestMove "g1f3"))

    [<Fact>]
    let ``parseLine: info lines are converted to Info messages`` () =
        let line = "info depth 10 score cp 13 pv e2e4"
        match UciProtocol.parseLine line with
        | Some (Info analysis) -> 
            analysis.Depth |> should equal 10
            analysis.Pv |> should contain "e2e4"
        | _ -> failwith "Should have parsed as Info"

    [<Fact>]
    let ``parseLine: ignores unknown engine output`` () =
        UciProtocol.parseLine "option name Hash type spin" |> should equal None
        UciProtocol.parseLine "id name Stockfish" |> should equal None

    [<Fact>]
    let ``Integration: Engine responds to isready`` () =
        let mutable receivedReady = false
        let onMsg = function
            | Ready -> receivedReady <- true
            | _ -> ()
        
        let engine = Engine.spawn @"D:\Github\Grampus\stockfish.exe" onMsg
        System.Threading.Thread.Sleep(1000) // Give it a second
    
        receivedReady |> should be True
        engine.Post Quit