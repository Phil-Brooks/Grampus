namespace Grampus

open System.Text.RegularExpressions

module pMove =
    let [<Literal>] Simple = 0
    let [<Literal>] Capture = 1
    let [<Literal>] CastleKingSide = 2
    let [<Literal>] CastleQueenSide = 3
    
    let CreateAll(mt, tgs, pc, orf, orr, pp, ic, id, im, san) =
        { Mtype = mt
          TargetSquare = tgs
          Piece = pc
          OriginFile = orf
          OriginRank = orr
          PromotedPiece = pp
          IsCheck = ic
          IsDoubleCheck = id
          IsCheckMate = im
          San = san }
    
    let CreateOrig(mt, tgs, pc, orf, orr, san) =
        CreateAll(mt, tgs, pc, orf, orr, None, false, false, false, san)
    let Create(mt, tgs, pc, san) = CreateOrig(mt, tgs, pc, None, None, san)
    let CreateCastle(mt, san) =
        CreateOrig(mt, OUTOFBOUNDS, Some(PieceType.King), None, None, san)
    
    let Parse(s : string) =
        let (|SimpleMove|Castle|PawnCapture|AmbiguousFile|AmbiguousRank|Promotion|PromCapture|) (s:string) =
            if Regex.IsMatch(s, "^[BNRQK][a-h][1-8]$") then 
                SimpleMove(s.[0] |> PieceType.fromChar, s.[1..] |> Square.Parse)
            elif Regex.IsMatch(s, "^[a-h][1-8]$") then 
                SimpleMove(PieceType.Pawn, s |> Square.Parse)
            elif s = "O-O" then Castle('K')
            elif s = "O-O-O" then Castle('Q')
            elif Regex.IsMatch(s, "^[a-h][a-h][1-8]$") then 
                PawnCapture(s.[0] |> File.fromChar, s.[1..] |> Square.Parse)
            elif Regex.IsMatch(s, "^[BNRQK][a-h][a-h][1-8]$") then 
                AmbiguousFile(s.[0] |> PieceType.fromChar, s.[1] |> File.fromChar, s.[2..] |> Square.Parse)
            elif Regex.IsMatch(s, "^[BNRQK][1-8][a-h][1-8]$") then 
                AmbiguousRank(s.[0] |> PieceType.fromChar, s.[1] |> Rank.fromChar, s.[2..] |> Square.Parse)
            elif Regex.IsMatch(s, "^[a-h][1-8][BNRQ]$") then 
                Promotion(s.[0..1] |> Square.Parse, s.[2] |> PieceType.fromChar)
            elif Regex.IsMatch(s, "^[a-h][a-h][1-8][BNRQ]$") then 
                PromCapture(s.[0] |> File.fromChar, s.[1..2] |> Square.Parse, s.[3] |> PieceType.fromChar)
            else failwith ("invalid move: " + s)
        
        let strip chars = String.collect (fun c -> if Seq.exists ((=) c) chars then "" else string c)
        let m = s |> strip "+x#=" |> fun x -> x.Replace("e.p.", "")
        
        match m with
        | SimpleMove(p, sq) -> Create((if s.Contains("x") then Capture else Simple), sq, Some(p), s)
        | Castle(c) -> CreateCastle((if c = 'K' then CastleKingSide else CastleQueenSide), s)
        | PawnCapture(f, sq) -> CreateOrig(Capture, sq, Some(PieceType.Pawn), Some(f), None, s)
        | AmbiguousFile(p, f, sq) -> CreateOrig((if s.Contains("x") then Capture else Simple), sq, Some(p), Some(f), None, s)
        | AmbiguousRank(p, r, sq) -> CreateOrig((if s.Contains("x") then Capture else Simple), sq, Some(p), None, Some(r), s)
        | Promotion(sq, p) -> CreateAll(Simple, sq, Some(PieceType.Pawn), None, None, Some(p), false, false, false, s)
        | PromCapture(f, sq, p) -> CreateAll(Capture, sq, Some(PieceType.Pawn), Some(f), None, Some(p), false, false, false, s)

