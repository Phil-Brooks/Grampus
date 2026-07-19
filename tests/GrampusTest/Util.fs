namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
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

