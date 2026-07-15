namespace Grampus

module Square =
    let Parse(s : string) =
        if s.Length <> 2 then failwith (s + " is not a valid position")
        else 
            let file = File.fromChar(s.[0])
            let rank = Rank.fromChar(s.[1])
            Sq(file, rank)
    
    let IsInBounds(pos : int) = pos >= 0 && pos <= 63
    let ToRank(pos : int) : int = pos / 8
    let ToFile(pos : int) : int = pos % 8
    
    let DirectionTo (pto : int) (pfrom : int) =
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
    
    let PositionInDirectionUnsafe (dir : Dirn) (pos : int) : int =
        pos + int (dir)
    
    let PositionInDirection (dir : Dirn) (pos : int) =
        if not (pos |> IsInBounds) then OUTOFBOUNDS
        else 
            let f = pos |> ToFile
            let r = pos |> ToRank
            
            let nr, nf =
                match dir with
                | Dirn.DirN -> r + 1, f
                | Dirn.DirE -> r, f + 1
                | Dirn.DirS -> r - 1, f
                | Dirn.DirW -> r, f - 1
                | Dirn.DirNE -> r + 1, f + 1
                | Dirn.DirSE -> r - 1, f + 1
                | Dirn.DirSW -> r - 1, f - 1
                | Dirn.DirNW -> r + 1, f - 1
                | Dirn.DirNNE -> r + 2, f + 1
                | Dirn.DirEEN -> r + 1, f + 2
                | Dirn.DirEES -> r - 1, f + 2
                | Dirn.DirSSE -> r - 2, f + 1
                | Dirn.DirSSW -> r - 2, f - 1
                | Dirn.DirWWS -> r - 1, f - 2
                | Dirn.DirWWN -> r + 1, f - 2
                | Dirn.DirNNW -> r + 2, f - 1
                | _ -> Rank.EMPTY, File.EMPTY
            if nr = Rank.EMPTY && nf = File.EMPTY then OUTOFBOUNDS
            elif (nr |> Rank.IsInBounds) && (nf |> File.IsInBounds) then 
                Sq(nf, nr)
            else OUTOFBOUNDS
    
    let ToBitboard(pos : int) =
        if pos |> IsInBounds then (1UL <<< int (pos)) |> BitB
        else Bitboard.Empty
    
    let Between (pto : int) (pfrom : int) =
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
