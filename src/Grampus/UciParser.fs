namespace Grampus

open System

module UciParser =
    let parseScore (parts: string[]) =
        let idx = parts |> Array.tryFindIndex (fun x -> x = "score")
        match idx with
        | Some i when parts.[i+1] = "cp" -> Centipawns (int parts.[i+2])
        | Some i when parts.[i+1] = "mate" -> MateIn (int parts.[i+2])
        | _ -> Unknown

    let parseInfo (line: string) : Analysis option =
        if not (line.StartsWith "info") || not (line.Contains "pv") then None
        else
            let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let getVal key = 
                parts |> Array.tryFindIndex ((=) key) 
                |> Option.map (fun i -> parts.[i+1])

            let pvIdx = line.IndexOf(" pv ") + 4
            let pvParts = line.Substring(pvIdx).Split(' ') |> Array.toList

            Some {
                Depth = getVal "depth" |> Option.map int |> Option.defaultValue 0
                Nodes = getVal "nodes" |> Option.map int64 |> Option.defaultValue 0L
                Score = parseScore parts
                Pv = pvParts
            }