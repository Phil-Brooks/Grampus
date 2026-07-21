namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module HistoryLogic =

    [<Fact>]
    let ``First move by White adds a new row`` () =
        let bd = Board.Start // White to move, Fullmove 1
        let actions = HistoryLogic.getRequiredActions bd "e4" 0
        
        // Should return a list containing exactly one AddNewRow action
        actions |> should equal [ AddNewRow(1, "e4") ]

    [<Fact>]
    let ``Response by Black updates existing row`` () =
        // Board after 1. e4 (It is now Black's turn)
        // Note: Fullmove remains 1 until Black completes their turn
        let bd = { Board.Start with WhosTurn = 1 } 
        let actions = HistoryLogic.getRequiredActions bd "e5" 1
        
        // Should return a list containing exactly one Update action
        actions |> should equal [ UpdateExistingRow "e5" ]

    [<Fact>]
    let ``Starting from FEN as Black returns two actions to prevent index crash`` () =
        // Board where it's move 10, Black to move, but our UI grid is empty
        let bd = { Board.Start with WhosTurn = 1; Fullmove = 10 }
        
        let actions = HistoryLogic.getRequiredActions bd "Nf6" 0
        
        // Should return TWO actions: 
        // 1. Create the row with a placeholder for white
        // 2. Fill in the black column
        actions |> should equal [ AddNewRow(10, "..."); UpdateExistingRow "Nf6" ]

    [<Fact>]
    let ``Subsequent White move adds a new row`` () =
        // Board after 1. e4 e5 (It is now White's turn, move 2)
        let bd = { Board.Start with WhosTurn = 0; Fullmove = 2 }
        let actions = HistoryLogic.getRequiredActions bd "d4" 1
        
        actions |> should equal [ AddNewRow(2, "d4") ]