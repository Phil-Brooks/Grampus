namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module File =

    // --- 1. Parsing Tests ---

    [<Theory>]
    [<InlineData('a', 0)>] // FileA
    [<InlineData('h', 7)>] // FileH
    [<InlineData('d', 3)>] // FileD
    let ``Parse returns correct internal index for valid lowercase characters`` (c: char, expected: int) =
        File.fromChar c |> should equal expected

    [<Theory>]
    [<InlineData('z')>]
    [<InlineData('1')>]
    [<InlineData(' ')>]
    let ``Parse throws exception for invalid file characters`` (invalidChar: char) =
        (fun () -> File.fromChar invalidChar |> ignore) |> should throw typeof<System.Exception>

    // --- 2. Bounds Checking ---

    [<Theory>]
    [<InlineData(0s, true)>]
    [<InlineData(7s, true)>]
    [<InlineData(-1s, false)>]
    [<InlineData(8s, false)>]
    let ``IsInBounds correctly identifies valid and invalid indices`` (f: int, expected: bool) =
        File.IsInBounds f |> should equal expected
