namespace Grampus

open System.Text
open System.Text.RegularExpressions

module FEN =
    let Parse(sFEN : string) =
        let pieceat = Array.create 64 Piece.EMPTY
        let sbPattern = new StringBuilder()
        sbPattern.Append(@"(?<R8>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R7>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R6>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R5>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R4>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R3>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R2>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R1>[\w]{1,8})") |> ignore
        sbPattern.Append(@"\s+(?<Player>[wbWB]{1})") |> ignore
        sbPattern.Append(@"\s+(?<Castle>[-KQkq]{1,4})") |> ignore
        sbPattern.Append(@"\s+(?<Enpassant>[-\w]{1,2})") |> ignore
        sbPattern.Append(@"\s+(?<FiftyMove>[-\d]+)") |> ignore
        sbPattern.Append(@"\s+(?<FullMove>[-\d]+)") |> ignore
        let pattern = sbPattern.ToString()
        let regex = new Regex(pattern)
        let matches = regex.Matches(sFEN)
        if matches.Count = 0 then failwith "No valid fen found"
        if matches.Count > 1 then failwith "Multiple FENs in string"
        let matchr = matches.[0]
        let sRanks =
            RANKS 
            |> List.map 
                   (fun r -> 
                   matchr.Groups.["R" + (r |> Rank.RankToString)].Value)
        let sPlayer = matchr.Groups.["Player"].Value
        let sCastle = matchr.Groups.["Castle"].Value
        let sEnpassant = matchr.Groups.["Enpassant"].Value
        let sFiftyMove = matchr.Groups.["FiftyMove"].Value
        let sFullMove = matchr.Groups.["FullMove"].Value
        for rank in RANKS do
            let rec getpc (cl : char list) ifl =
                if not (List.isEmpty cl) then 
                    if ifl > 7 then 
                        failwith 
                            ("too many pieces in rank " 
                             + (rank |> Rank.RankToString))
                    let c = cl.Head
                    if "1234567890".IndexOf(c) >= 0 then 
                        getpc cl.Tail (ifl + System.Int32.Parse(c.ToString()))
                    else 
                        pieceat.[int (Sq(FILES.[ifl], rank))] <- Piece.Parse(c) //OK
                        getpc cl.Tail (ifl + 1)
            
            let srank =
                sRanks.[System.Int32.Parse(rank |> Rank.RankToString) - 1]
            getpc (srank.ToCharArray() |> List.ofArray) 0
        let whosTurn =
            if sPlayer = "w" then Player.White
            elif sPlayer = "b" then Player.Black
            else failwith (sPlayer + " is not a valid player")
        
        let castleWS = sCastle.IndexOf("K") >= 0
        let castleWL = sCastle.IndexOf("Q") >= 0
        let castleBS = sCastle.IndexOf("k") >= 0
        let castleBL = sCastle.IndexOf("q") >= 0
        
        let enpassant =
            if sEnpassant <> "-" then Square.Parse(sEnpassant)
            else OUTOFBOUNDS
        
        let fiftyMove =
            if sFiftyMove <> "-" then System.Int32.Parse(sFiftyMove)
            else 0
        
        let fullMove =
            if sFullMove <> "-" then System.Int32.Parse(sFullMove)
            else 0
        
        { Pieceat = pieceat |> List.ofArray
          Whosturn = whosTurn
          CastleWS = castleWS
          CastleWL = castleWL
          CastleBS = castleBS
          CastleBL = castleBL
          Enpassant = enpassant
          Fiftymove = fiftyMove
          Fullmove = fullMove }
    
    let StartStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let Start = Parse StartStr
