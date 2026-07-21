namespace Grampus

open System
open System.Net.Http
open System.Text.Json
open System.Text.Json.Serialization

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

module LichessClient =
    open System.Net.Http
    open System.Text.Json

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
        let token = getApiToken()
        use request = createRequest token fen
        try
            let! response = client.SendAsync(request) |> Async.AwaitTask
            if response.IsSuccessStatusCode then
                let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                return parseResponse content
            else return None
        with _ -> return None
    }