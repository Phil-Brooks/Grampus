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

//TODO: unit test
module LichessClient =
    let private client = new HttpClient()
    let private apiToken = 
        Environment.GetEnvironmentVariable("LICHESS_API_TOKEN") 
        |> Option.ofObj
        |> function
           | Some token -> token
           | None -> failwith "LICHESS_API_TOKEN environment variable is not set!"    
    let fetchMastersStats (fen: string) = async {
        let url = sprintf "https://explorer.lichess.ovh/masters?fen=%s" (System.Uri.EscapeDataString fen)
        
        use request = new HttpRequestMessage(HttpMethod.Get, url)
        
        // 1. Mandatory User-Agent
        request.Headers.Add("User-Agent", "Grampus-Chess-UI (Contact: your-email@example.com)")
        
        // 2. NEW: Authorization Header
        request.Headers.Add("Authorization", sprintf "Bearer %s" apiToken)

        try
            let! response = client.SendAsync(request) |> Async.AwaitTask
            
            if response.IsSuccessStatusCode then
                let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                let data = JsonSerializer.Deserialize<MasterResponse>(content)
                return Some data
            else
                // Log the error if it still fails
                let! err = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                printfn "Lichess API Error (%O): %s" response.StatusCode err
                return None
        with ex -> 
            printfn "Network Error: %s" ex.Message
            return None
    }