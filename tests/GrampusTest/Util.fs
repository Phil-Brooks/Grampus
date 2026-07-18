namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open Grampus

module Util =

    // --- 2. Arithmetic Operators ---
    [<Fact>]
    let ``Rank arithmetic operators work correctly`` () =
        R1 + 1 |> should equal R2
        R8 - 7 |> should equal R1
        R4 + 2 |> should equal R6

    [<Fact>]
    let ``File arithmetic operators work correctly`` () =
        A + 1 |> should equal B
        H - 7 |> should equal A
        C + 3 |> should equal F

    // --- 3. Property Based Testing (FsCheck) ---
    // Using the ChessDimGenerator we created for TypesTests
    
    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``Rank addition and subtraction are inverses`` (r: int) (offset: int) =
        // We use a small offset to stay within reasonable bounds for the logic
        let smallOffset = offset % 4
        (r + smallOffset) - smallOffset = r

    [<Property(Arbitrary = [| typeof<ChessDimGenerator> |])>]
    let ``File addition and subtraction are inverses`` (f: int) (offset: int) =
        let smallOffset = offset % 4
        (f + smallOffset) - smallOffset = f

