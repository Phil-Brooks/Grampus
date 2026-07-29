namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus
open System.IO
open System

module LichessClient =

    let testResponse = {
        White = 10; Draws = 5; Black = 5;
        Moves = [| { San = "e4"; White = 5; Draws = 2; Black = 1; AvgRating = 2500 } |]
    }

    // A helper to ensure we are testing in a clean state
    let clearTestCache () =
        let path = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "GrampusChess", "LichessCache")
        if Directory.Exists path then
            Directory.GetFiles(path) |> Array.iter File.Delete
    

    [<Fact>]
    let ``Cache roundtrip: Saving and then loading returns the same data`` () =
        clearTestCache()
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        
        MasterDataCache.saveToCache fen testResponse
        match MasterDataCache.tryGetCachedResponse fen with
        | Some (data, ts) -> 
            data.White |> should equal testResponse.White
            ts |> should be (lessThanOrEqualTo DateTime.Now)
        | None -> failwith "Should have returned cached data"
    
    [<Fact>]
    let ``Normalization: Different move clocks hit the same cache entry`` () =
        clearTestCache()
        // These two FENs represent the same position but different move clocks (0 1 vs 5 10)
        let fen1 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let fen2 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 5 10"
        
        MasterDataCache.saveToCache fen1 testResponse
        
        // Should be able to retrieve using fen2
        let result = MasterDataCache.tryGetCachedResponse fen2
        result.IsSome |> should be True

    [<Fact>]
    let ``Expiration: Returns None and deletes file if older than 7 days`` () =
        clearTestCache()
        let fen = "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
        MasterDataCache.saveToCache fen testResponse
        
        // Manually manipulate the file timestamp to 8 days ago
        let cacheDir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "GrampusChess", "LichessCache")
        let file = Directory.GetFiles(cacheDir) |> Array.head
        File.SetLastWriteTime(file, DateTime.Now.AddDays(-8.0))
        
        let result = MasterDataCache.tryGetCachedResponse fen
        
        result |> should equal None
        File.Exists(file) |> should be False // Should have been cleaned up

    [<Fact>]
    let ``parseResponse handles empty move list correctly`` () =
        let json = """{ "white": 0, "draws": 0, "black": 0, "moves": [] }"""
        let result = LichessClient.parseResponse json
        result.IsSome |> should be True
        result.Value.Moves.Length |> should equal 0
    
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