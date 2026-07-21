namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module Piece =
    
    // --- 1. Exhaustive Unit Tests (Theory) ---
    
    [<Theory>]
    [<InlineData('P', WPAWN)>]
    [<InlineData('k', BKING)>]
    [<InlineData('n', BKNIGHT)>]
    [<InlineData('.', EMPTY)>]
    let ``FromStr returns correct Piece for valid char``(input: char, expected: int) =
        Piece.FromStr input |> should equal expected

    [<Fact>]
    let ``FromStr throws exception for invalid char``() =
        (fun () -> Piece.FromStr 'Z' |> ignore) |> should throw typeof<System.Exception>

    [<Theory>]
    [<InlineData(WPAWN, "P")>]
    [<InlineData(BQUEEN, "q")>]
    [<InlineData(EMPTY, ".")>]
    let ``ToStr returns correct string for Piece``(input, expected) =
        Piece.ToStr input |> should equal expected

