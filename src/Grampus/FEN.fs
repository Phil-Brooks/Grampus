namespace Grampus

open System.Text
open System.Text.RegularExpressions

module FEN =
    let getMatch(sFEN : string) =
        let sbPattern = new StringBuilder()
        sbPattern.Append(@"(?<R8>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R7>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R6>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R5>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R4>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R3>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R2>[\w]{1,8})/") |> ignore
        sbPattern.Append(@"(?<R1>[\w]{1,8})") |> ignore
        sbPattern.Append(@"\s+(?<Colour>[wbWB]{1})") |> ignore
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
        matchr
    let ToBrd(sFEN : string) =
        let pieceat = Array.create 64 EMPTY
        let mutable wtKingPos = OUTOFBOUNDS
        let mutable bkKingPos = OUTOFBOUNDS
        let matchr = getMatch(sFEN)
        for r = 1 to 8 do
            let srank = matchr.Groups.["R" + r.ToString()].Value
            let rec getpc (cl : char list) ifl =
                if not (List.isEmpty cl) then 
                    if ifl > 7 then failwith ("too many pieces in rank R" + r.ToString())
                    let c = cl.Head
                    if "1234567890".IndexOf(c) >= 0 then 
                        getpc cl.Tail (ifl + System.Int32.Parse(c.ToString()))
                    else 
                        let sq = SQ(ifl, r - 1)
                        let pc = Piece.Parse(c)
                        pieceat.[sq] <- pc
                        if pc = WKING then wtKingPos <- sq
                        if pc = BKING then bkKingPos <- sq
                        getpc cl.Tail (ifl + 1)
            getpc (srank.ToCharArray() |> List.ofArray) 0
        let whosTurn =
            let sColour = matchr.Groups.["Colour"].Value
            sColour |> Colour.FromStr
        let castleRts =
            let sCastle = matchr.Groups.["Castle"].Value
            sCastle|> Castle.FromStr
        let enPassant =
            let sEnpassant = matchr.Groups.["Enpassant"].Value
            if sEnpassant <> "-" then Square.Parse(sEnpassant) else OUTOFBOUNDS
        let fiftyMove =
            let sFiftyMove = matchr.Groups.["FiftyMove"].Value
            if sFiftyMove <> "-" then System.Int32.Parse(sFiftyMove) else 0
        let fullMove =
            let sFullMove = matchr.Groups.["FullMove"].Value
            if sFullMove <> "-" then System.Int32.Parse(sFullMove) else 0
        { 
          PieceAt = pieceat
          WtKingPos = wtKingPos
          BkKingPos = bkKingPos
          WhosTurn = whosTurn
          CastleRts = castleRts
          EnPassant = enPassant
          Fiftymove = fiftyMove
          Fullmove = fullMove
        }
    let StartStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let FromBrd(bd: Brd) =
        let ranks = Rank.List |> List.rev
        let files = File.List
        let fenRanks = ranks |> List.map (fun r ->
            let mutable emptyCount = 0
            let mutable rankStr = ""
            for f in files do
                let pc = bd.PieceAt.[SQ(f, r)]
                if pc = EMPTY then 
                    emptyCount <- emptyCount + 1
                else
                    if emptyCount > 0 then 
                        rankStr <- rankStr + string emptyCount
                        emptyCount <- 0
                    rankStr <- rankStr + (pc|>Piece.ToStr)
            if emptyCount > 0 then rankStr <- rankStr + string emptyCount
            rankStr
        )
        let boardPart = String.concat "/" fenRanks
        let turn = bd.WhosTurn |> Colour.ToStr
        let castles = bd.CastleRts |> Castle.ToStr
        let ep = bd.EnPassant |> Square.ToStr
        sprintf "%s %s %s %s %d %d" boardPart turn castles ep bd.Fiftymove bd.Fullmove