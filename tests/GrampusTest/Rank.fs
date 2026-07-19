namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module Rank =

    // --- 1. Parsing and String Conversion ---

    [<Theory>]
    [<InlineData('1', 0s)>] // Rank1
    [<InlineData('8', 7s)>] // Rank8
    [<InlineData('4', 3s)>] // Rank4
    let ``Parse returns correct internal index for valid characters`` (c: char, expected: int) =
        Rank.fromChar c |> should equal expected

    [<Fact>]
    let ``Parse throws exception for invalid rank characters`` () =
        (fun () -> Rank.fromChar '9' |> ignore) |> should throw typeof<System.Exception>
        (fun () -> Rank.fromChar 'z' |> ignore) |> should throw typeof<System.Exception>

    [<Theory>]
    [<InlineData(0s, "1")>]
    [<InlineData(7s, "8")>]
    let ``RankToString returns correct chess notation`` (rank: int, expected: string) =
        Rank.ToStr rank |> should equal expected
