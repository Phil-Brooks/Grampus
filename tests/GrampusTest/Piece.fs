namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module Piece =
    
    [<Fact>]
    let ``ToStr tests``() =
        let ans = Piece.WKnight
        ans
        |> Piece.ToStr
        |> should equal "N"
        let ans = Piece.BKnight
        ans
        |> Piece.ToStr
        |> should equal "n"




