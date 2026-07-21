namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus
open System.Net.Http

module LichessClient =

    // --- 1. URL & Header Tests ---

    [<Fact>]
    let ``createRequest builds correct URL with escaped FEN`` () =
        // Arrange
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let token = "test_token"
    
        // Act
        use request = LichessClient.createRequest token fen
        let url = request.RequestUri.ToString()
        let userAgent = request.Headers.UserAgent.ToString()
    
        // Assert - Using .Contains(...) |> should be True
        url.Contains("https://explorer.lichess.ovh/masters") |> should be True
        url.Contains("fen=") |> should be True
        url.Contains("rnbqkbnr") |> should be True
    
        // Check Authorization Header
        request.Headers.Authorization.Parameter |> should equal "test_token"
    
        // Check UserAgent (Fixes the line 36 error)
        userAgent.Contains("Grampus-Chess-UI") |> should be True
    
    // --- 2. Parsing Tests ---

    [<Fact>]
    let ``parseResponse correctly deserializes Lichess JSON`` () =
        let sampleJson = """
        {
            "white": 100,
            "draws": 50,
            "black": 30,
            "moves": [
                { "san": "e4", "white": 50, "draws": 20, "black": 10, "averageRating": 2500 },
                { "san": "d4", "white": 30, "draws": 20, "black": 10, "averageRating": 2480 }
            ]
        }
        """
        let result = LichessClient.parseResponse sampleJson
        
        result.IsSome |> should be True
        let data = result.Value
        data.White |> should equal 100
        data.Moves.Length |> should equal 2
        data.Moves.[0].San |> should equal "e4"
        data.Moves.[0].AvgRating |> should equal 2500

    [<Fact>]
    let ``parseResponse returns None for invalid JSON`` () =
        let invalidJson = "{ \"error\": \"not found\" " // Missing closing brace
        let result = LichessClient.parseResponse invalidJson
        result |> should equal None

    // --- 3. Integration-style Test (Optional) ---
    // Only run this if you want to verify the actual environment variable exists
    [<Fact>]
    let ``Environment token is present`` () =
        let token = System.Environment.GetEnvironmentVariable("LICHESS_API_TOKEN")
        token |> should not' (equal null)