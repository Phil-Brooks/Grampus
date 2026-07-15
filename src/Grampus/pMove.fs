namespace Grampus

open System.Text.RegularExpressions

module pMove =
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
                SimpleMove(s.[0] |> PieceType.Parse, s.[1..] |> Square.Parse)
            elif Regex.IsMatch(s, "^[a-h][1-8]$") then 
                SimpleMove(PieceType.Pawn, s |> Square.Parse)
            elif s = "O-O" then Castle('K')
            elif s = "O-O-O" then Castle('Q')
            elif Regex.IsMatch(s, "^[a-h][a-h][1-8]$") then 
                PawnCapture(s.[0] |> File.fromChar, s.[1..] |> Square.Parse)
            elif Regex.IsMatch(s, "^[BNRQK][a-h][a-h][1-8]$") then 
                AmbiguousFile(s.[0] |> PieceType.Parse, s.[1] |> File.fromChar, s.[2..] |> Square.Parse)
            elif Regex.IsMatch(s, "^[BNRQK][1-8][a-h][1-8]$") then 
                AmbiguousRank(s.[0] |> PieceType.Parse, s.[1] |> Rank.fromChar, s.[2..] |> Square.Parse)
            elif Regex.IsMatch(s, "^[a-h][1-8][BNRQ]$") then 
                Promotion(s.[0..1] |> Square.Parse, s.[2] |> PieceType.Parse)
            elif Regex.IsMatch(s, "^[a-h][a-h][1-8][BNRQ]$") then 
                PromCapture(s.[0] |> File.fromChar, s.[1..2] |> Square.Parse, s.[3] |> PieceType.Parse)
            else failwith ("invalid move: " + s)
        
        let strip chars = String.collect (fun c -> if Seq.exists ((=) c) chars then "" else string c)
        let m = s |> strip "+x#=" |> fun x -> x.Replace("e.p.", "")
        
        match m with
        | SimpleMove(p, sq) -> Create((if s.Contains("x") then MoveType.Capture else MoveType.Simple), sq, Some(p), s)
        | Castle(c) -> CreateCastle((if c = 'K' then MoveType.CastleKingSide else MoveType.CastleQueenSide), s)
        | PawnCapture(f, sq) -> CreateOrig(MoveType.Capture, sq, Some(PieceType.Pawn), Some(f), None, s)
        | AmbiguousFile(p, f, sq) -> CreateOrig((if s.Contains("x") then MoveType.Capture else MoveType.Simple), sq, Some(p), Some(f), None, s)
        | AmbiguousRank(p, r, sq) -> CreateOrig((if s.Contains("x") then MoveType.Capture else MoveType.Simple), sq, Some(p), None, Some(r), s)
        | Promotion(sq, p) -> CreateAll(MoveType.Simple, sq, Some(PieceType.Pawn), None, None, Some(p), false, false, false, s)
        | PromCapture(f, sq, p) -> CreateAll(MoveType.Capture, sq, Some(PieceType.Pawn), Some(f), None, Some(p), false, false, false, s)

    /// MODERNIZED: Replaces the broken logic by filtering all legal moves
    let ToMove (bd : Brd) (pmv : pMove) =
        // 1. Get the candidate legal moves based on the Piece Type
        let legalMoves =
            match pmv.Piece with
            | Some PieceType.Pawn   -> MoveGenerate.PawnMoves bd
            | Some PieceType.Knight -> MoveGenerate.KnightMoves bd
            | Some PieceType.Bishop -> MoveGenerate.BishopMoves bd
            | Some PieceType.Rook   -> MoveGenerate.RookMoves bd
            | Some PieceType.Queen  -> MoveGenerate.QueenMoves bd
            | Some PieceType.King   -> 
                // Combine standard king moves and castling for filtering
                (MoveGenerate.KingMoves bd) @ (MoveGenerate.CastleMoves bd)
            | _ -> failwith "Invalid SAN: No piece type identified"

        // 2. Filter the legal moves to find the one that matches the SAN properties
        let candidates =
            legalMoves |> List.filter (fun mv ->
                // A. Match Destination Square
                // (Note: Castling destination is G1/G8 or C1/C8)
                let matchesTarget = 
                    match pmv.Mtype with
                    | MoveType.CastleKingSide  -> (Move.To mv = G1 || Move.To mv = G8)
                    | MoveType.CastleQueenSide -> (Move.To mv = C1 || Move.To mv = C8)
                    | _ -> Move.To mv = pmv.TargetSquare

                // B. Match Origin Ambiguity (e.g., 'd' in Ndf3 or '1' in R1a5)
                let matchesFile = 
                    match pmv.OriginFile with
                    | Some f -> (Move.From mv |> Square.ToFile) = f
                    | None -> true
                
                let matchesRank = 
                    match pmv.OriginRank with
                    | Some r -> (Move.From mv |> Square.ToRank) = r
                    | None -> true

                // C. Match Promotion (using your Move module helpers)
                let matchesPromo = 
                    match pmv.PromotedPiece with
                    | Some p -> Move.IsPromotion mv && Move.PromoteType mv = p
                    | None   -> not (Move.IsPromotion mv)

                matchesTarget && matchesFile && matchesRank && matchesPromo
            )

        // 3. Return the match
        match candidates with
        | [singleMatch] -> singleMatch
        | [] -> failwithf "No legal move matches: %s" pmv.San
        | _  -> failwithf "Ambiguous move: %s (Multiple legal moves match this SAN)" pmv.San