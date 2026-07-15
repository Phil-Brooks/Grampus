namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Grampus

module File =

    // --- 1. Parsing Tests ---

    [<Theory>]
    [<InlineData('a', 0s)>] // FileA
    [<InlineData('h', 7s)>] // FileH
    [<InlineData('d', 3s)>] // FileD
    let ``Parse returns correct internal index for valid lowercase characters`` (c: char, expected: File) =
        File.Parse c |> should equal expected

    [<Theory>]
    [<InlineData('A', 0s)>]
    [<InlineData('H', 7s)>]
    let ``Parse handles uppercase characters correctly`` (c: char, expected: File) =
        File.Parse c |> should equal expected

    [<Theory>]
    [<InlineData('z')>]
    [<InlineData('1')>]
    [<InlineData(' ')>]
    let ``Parse throws exception for invalid file characters`` (invalidChar: char) =
        (fun () -> File.Parse invalidChar |> ignore) |> should throw typeof<System.Exception>

    // --- 2. Bounds Checking ---

    [<Theory>]
    [<InlineData(0s, true)>]
    [<InlineData(7s, true)>]
    [<InlineData(-1s, false)>]
    [<InlineData(8s, false)>]
    let ``IsInBounds correctly identifies valid and invalid indices`` (f: File, expected: bool) =
        File.IsInBounds f |> should equal expected

    // --- 3. Property Based Testing ---

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Any valid File index can be round-tripped through the global FILE_NAMES list`` (f: File) =
        // Arrange: Get the character for this file from the global list defined in Types
        let fileChar = FILE_NAMES.[int f].[0]
        
        // Assert: Parsing that character should return the original index
        File.Parse fileChar = f

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``IsInBounds is always true for generated valid files`` (f: File) =
        File.IsInBounds f = true

    [<Property>]
    let ``IsInBounds is always false for values outside 0 to 7`` (i: int) =
        if i < 0 || i > 7 then
            File.IsInBounds i = false
        else true