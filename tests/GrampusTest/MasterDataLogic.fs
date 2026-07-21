namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus.MasterDataLogic

module MasterDataLogic =

    // --- 1. formatCount Tests ---

    [<Theory>]
    [<InlineData(0, "0")>]
    [<InlineData(999, "999")>]
    [<InlineData(1000, "1.0k")>]
    [<InlineData(1500, "1.5k")>]
    [<InlineData(99900, "99.9k")>]
    [<InlineData(1000000, "1.0m")>]
    [<InlineData(2500000, "2.5m")>]
    let ``formatCount correctly shortens large numbers`` (input: int, expected: string) =
        formatCount input |> should equal expected

    // --- 2. calculateRatios Tests ---

    [<Fact>]
    let ``calculateRatios handles division by zero (empty stats)`` () =
        let result = calculateRatios 0 0 0
        result.WhitePct |> should equal 0.0
        result.DrawPct |> should equal 0.0
        result.BlackPct |> should equal 0.0

    [<Fact>]
    let ``calculateRatios returns correct percentages for equal distribution`` () =
        let result = calculateRatios 10 10 10
        // We use approximately equal for floating point if necessary, 
        // but 1/3 is usually fine with standard equality in simple cases.
        Assert.Equal(0.333, result.WhitePct, 3)
        Assert.Equal(0.333, result.DrawPct, 3)
        Assert.Equal(0.333, result.BlackPct, 3)

    [<Fact>]
    let ``calculateRatios returns exact percentages for clean divisions`` () =
        let result = calculateRatios 50 25 25 // Total 100
        result.WhitePct |> should equal 0.5
        result.DrawPct |> should equal 0.25
        result.BlackPct |> should equal 0.25

    [<Theory>]
    [<InlineData(100, 0, 0, 1.0, 0.0, 0.0)>]
    [<InlineData(0, 100, 0, 0.0, 1.0, 0.0)>]
    [<InlineData(0, 0, 100, 0.0, 0.0, 1.0)>]
    let ``calculateRatios handles 100 percent cases`` (w, d, b, expW, expD, expB) =
        let result = calculateRatios w d b
        result.WhitePct |> should equal expW
        result.DrawPct |> should equal expD
        result.BlackPct |> should equal expB