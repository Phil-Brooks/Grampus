namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module Board =

    [<Fact>]
    let ``Load start to Posn``() =
        let ans = Board.Start
        ans.EnPassant
        |> should equal 64s
