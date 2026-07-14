namespace GrampusInternal

open Grampus

module Square =
    let Parse(s : string) =
        if s.Length <> 2 then failwith (s + " is not a valid position")
        else 
            let file = File.Parse(s.[0])
            let rank = Rank.Parse(s.[1])
            Sq(file, rank)
    
    let IsInBounds(pos : Square) = int (pos) >= 0 && int (pos) <= 63
    let ToRank(pos : Square) : Rank = pos / 8s
    let ToFile(pos : Square) : File = pos % 8s
    
    let DirectionTo (pto : Square) (pfrom : Square) =
        let rankfrom = int (pfrom |> ToRank)
        let filefrom = int (pfrom |> ToFile)
        let rankto = int (pto |> ToRank)
        let fileto = int (pto |> ToFile)
        if fileto = filefrom then 
            if rankfrom < rankto then Dirn.DirN
            else Dirn.DirS
        elif rankfrom = rankto then 
            if filefrom > fileto then Dirn.DirW
            else Dirn.DirE
        else 
            let rankchange = rankto - rankfrom
            let filechange = fileto - filefrom
            
            let rankchangeabs =
                if rankchange > 0 then rankchange
                else -rankchange
            
            let filechangeabs =
                if filechange > 0 then filechange
                else -filechange
            
            if (rankchangeabs = 1 && filechangeabs = 2) 
               || (rankchangeabs = 2 && filechangeabs = 1) then 
                ((rankchange * 8) + filechange) |> enum<Dirn>
            elif rankchangeabs <> filechangeabs then 0 |> enum<Dirn>
            elif rankchange < 0 then 
                if filechange > 0 then Dirn.DirSE
                else Dirn.DirSW
            else if filechange > 0 then Dirn.DirNE
            else Dirn.DirNW
    
    let PositionInDirectionUnsafe (dir : Dirn) (pos : Square) : Square =
        pos + int16 (dir)
    
    let PositionInDirection (dir : Dirn) (pos : Square) =
        if not (pos |> IsInBounds) then OUTOFBOUNDS
        else 
            let f = pos |> ToFile
            let r = pos |> ToRank
            
            let nr, nf =
                match dir with
                | Dirn.DirN -> r -! 1s, f
                | Dirn.DirE -> r, f ++ 1s
                | Dirn.DirS -> r +! 1s, f
                | Dirn.DirW -> r, f -- 1s
                | Dirn.DirNE -> r -! 1s, f ++ 1s
                | Dirn.DirSE -> r +! 1s, f ++ 1s
                | Dirn.DirSW -> r +! 1s, f -- 1s
                | Dirn.DirNW -> r -! 1s, f -- 1s
                | Dirn.DirNNE -> r -! 2s, f ++ 1s
                | Dirn.DirEEN -> r -! 1s, f ++ 2s
                | Dirn.DirEES -> r +! 1s, f ++ 2s
                | Dirn.DirSSE -> r +! 2s, f ++ 1s
                | Dirn.DirSSW -> r +! 2s, f -- 1s
                | Dirn.DirWWS -> r +! 1s, f -- 2s
                | Dirn.DirWWN -> r -! 1s, f -- 2s
                | Dirn.DirNNW -> r -! 2s, f -- 1s
                | _ -> RANK_EMPTY, FILE_EMPTY
            if nr = RANK_EMPTY && nf = FILE_EMPTY then OUTOFBOUNDS
            elif (nr |> Rank.IsInBounds) && (nf |> File.IsInBounds) then 
                Sq(nf, nr)
            else OUTOFBOUNDS
    
    let ToBitboard(pos : Square) =
        if pos |> IsInBounds then (1UL <<< int (pos)) |> BitB
        else Bitboard.Empty
    
    let Between (pto : Square) (pfrom : Square) =
        let dir = pfrom |> DirectionTo(pto)
        
        let rec getb f rv =
            if f = pto then rv
            else 
                let nf = f |> PositionInDirectionUnsafe(dir)
                let nrv = rv ||| (nf |> ToBitboard)
                getb nf nrv
        
        let rv =
            if int (dir) = 0 then Bitboard.Empty
            else getb pfrom Bitboard.Empty
        
        rv &&& ~~~(pto |> ToBitboard)
