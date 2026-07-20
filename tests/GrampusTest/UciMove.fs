namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module UciMove =

    // Helper to create a board with a piece at a specific square
    // You may need to adjust this depending on how your Brd type is actually defined
    let createTestBoard (pieces: (int * int) list) (epSquare: int) =
        let mutable bd = {Board.EMP with PieceAt=Board.EMP.PieceAt|>Array.copy}
        for (sq, pc) in pieces do
            bd.PieceAt.[sq] <- pc
        { bd with EnPassant = epSquare }

    [<Fact>]
    let ``fromString returns None for invalid string length`` () =
        let bd = Board.Start
        UciMove.fromString bd "e2e" |> should equal None
        UciMove.fromString bd "" |> should equal None

    [<Fact>]
    let ``fromString parses a standard quiet move`` () =
        // Setup: White Pawn on E2
        let e2, e4 = 12, 28 // Replace with your actual Square constants/indices
        let bd = createTestBoard [(e2, WPAWN)] -1
        
        let result = UciMove.fromString bd "e2e4"
        
        result.IsSome |> should be True
        let m = result.Value
        m.From |> should equal e2
        m.To |> should equal e4
        m.Pc |> should equal WPAWN
        m.CapPc |> should equal EMPTY
        m.Prom |> should equal EMPTY

    [<Fact>]
    let ``fromString parses a capture correctly`` () =
        // Setup: White Knight on f3, Black Pawn on d4
        let f3, d4 = 21, 27
        let bd = createTestBoard [(f3, WKNIGHT); (d4, BPAWN)] -1
        
        let result = UciMove.fromString bd "f3d4"
        
        let m = result.Value
        m.Pc|> should equal WKNIGHT
        m.CapPc |> should equal BPAWN

    [<Theory>]
    [<InlineData("a7a8q", WQUEEN)>]
    [<InlineData("a7a8r", WROOK)>]
    [<InlineData("a7a8b", WBISHOP)>]
    [<InlineData("a7a8n", WKNIGHT)>]
    let ``fromString parses white promotions`` (uci: string, expectedPromo: int) =
        let a7, a8 = 48, 56
        let bd = createTestBoard [(a7, WPAWN)] -1
        
        let result = UciMove.fromString bd uci
        
        result.Value.Prom |> should equal expectedPromo

    [<Fact>]
    let ``fromString identifies En Passant correctly`` () =
        // Setup: White pawn on e5, Black pawn just moved to d5 (EP square is d6)
        let e5, d6 = 36, 43
        let bd = createTestBoard [(e5, WPAWN)] d6
        
        let result = UciMove.fromString bd "e5d6"
        
        result.IsSome |> should be True
        let m = result.Value
        // Based on your code: Some (Move.CreateEp fromSq toSq pc epCapPc)
        m.To |> should equal d6
        m.CapPc |> should equal BPAWN // CreateEp sets this as the EP target

    [<Fact>]
    let ``fromString handles black promotion to knight`` () =
        let b2, b1 = 9, 1
        let bd = createTestBoard [(b2, BPAWN)] -1
        
        let result = UciMove.fromString bd "b2b1n"
        
        result.Value.Prom |> should equal BKNIGHT