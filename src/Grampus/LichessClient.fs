namespace Grampus

open System
open System.Net.Http
open System.Text.Json
open System.Text.Json.Serialization
open System.IO
open System.Security.Cryptography
open System.Text

type MasterMove = {
    [<JsonPropertyName("san")>] San : string
    [<JsonPropertyName("white")>] White : int
    [<JsonPropertyName("draws")>] Draws : int
    [<JsonPropertyName("black")>] Black : int
    [<JsonPropertyName("averageRating")>] AvgRating : int
}

type MasterResponse = {
    [<JsonPropertyName("white")>] White : int
    [<JsonPropertyName("draws")>] Draws : int
    [<JsonPropertyName("black")>] Black : int
    [<JsonPropertyName("moves")>] Moves : MasterMove[]
}

type MasterDataResult = {
    Data: MasterResponse
    IsCached: bool
    Timestamp: DateTime
}

module MasterDataCache =
    let private cacheDir = 
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "GrampusChess", "LichessCache")

    // Ensure directory exists
    let private ensureCacheDir () =
        if not (Directory.Exists cacheDir) then Directory.CreateDirectory cacheDir |> ignore

    // FENs contain '/', which are illegal in filenames. Use a hash.
    let private getCacheFileName (fen: string) =
        // Lichess ignores the half-move clock and full-move number for opening stats.
        // We normalize the FEN by taking only the first 4 parts.
        let normalizedFen = fen.Split(' ') |> Array.truncate 4 |> String.concat " "
        let bytes = Encoding.UTF8.GetBytes(normalizedFen)
        let hash = SHA256.HashData(bytes)
        Convert.ToHexString(hash) + ".json"

    let tryGetCachedResponse (fen: string) : (MasterResponse * DateTime) option =
        try
            ensureCacheDir()
            let filePath = Path.Combine(cacheDir, getCacheFileName fen)
        
            if File.Exists filePath then
                let lastWrite = File.GetLastWriteTime filePath
                if DateTime.Now.Subtract(lastWrite).TotalDays < 7.0 then
                    let json = File.ReadAllText filePath
                    let data = JsonSerializer.Deserialize<MasterResponse>(json)
                    Some (data, lastWrite) // Return both data and time
                else
                    File.Delete filePath
                    None
            else None
        with _ -> None    

    let saveToCache (fen: string) (data: MasterResponse) =
        try
            ensureCacheDir()
            let filePath = Path.Combine(cacheDir, getCacheFileName fen)
            let json = JsonSerializer.Serialize(data)
            File.WriteAllText(filePath, json)
        with _ -> ()


module LichessClient =
    // 1. Logic: Build the request
    let createRequest (token: string) (fen: string) =
        let url = sprintf "https://explorer.lichess.ovh/masters?fen=%s" (System.Uri.EscapeDataString fen)
        let request = new HttpRequestMessage(HttpMethod.Get, url)
        request.Headers.Add("User-Agent", "Grampus-Chess-UI (Contact: your-email@example.com)")
        request.Headers.Add("Authorization", sprintf "Bearer %s" token)
        request
    
    // 2. Logic: Parse the JSON 
    let parseResponse (json: string) =
        try
            JsonSerializer.Deserialize<MasterResponse>(json) |> Some
        with _ -> None

    // 3. Execution: The live client (Uses the logic above)
    let private client = new HttpClient()
    
    // Use a function for the token so it doesn't crash during unit testing 
    // of other modules if the variable isn't set.
    let getApiToken () = 
        Environment.GetEnvironmentVariable("LICHESS_API_TOKEN") 
        |> Option.ofObj 
        |> Option.defaultValue "no-token-set"

    let fetchMastersStats (fen: string) = async {
        match MasterDataCache.tryGetCachedResponse fen with
        | Some (data, timestamp) -> // Updated Cache to return the file date
            return Some { Data = data; IsCached = true; Timestamp = timestamp }
        | None ->
            let token = getApiToken()
            use request = createRequest token fen
            try
                let! response = client.SendAsync(request) |> Async.AwaitTask
                if response.IsSuccessStatusCode then
                    let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                    match parseResponse content with
                    | Some data ->
                        MasterDataCache.saveToCache fen data
                        return Some { Data = data; IsCached = false; Timestamp = DateTime.Now }
                    | None -> return None
                else return None
            with _ -> return None
    }
