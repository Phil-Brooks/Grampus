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
            if rankfrom < rankto then Dirn.N
            else Dirn.S
        elif rankfrom = rankto then 
            if filefrom > fileto then Dirn.W
            else Dirn.E
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
                ((rankchange * 8) + filechange) 
            elif rankchangeabs <> filechangeabs then 0 
            elif rankchange < 0 then 
                if filechange > 0 then Dirn.SE
                else Dirn.SW
            else if filechange > 0 then Dirn.NE
            else Dirn.NW
    
    let PositionInDirectionUnsafe (dir : int) (pos : int) : int =
        pos + dir
    
    let PositionInDirection (dir : int) (pos : int) =
        if not (pos |> IsInBounds) then OUTOFBOUNDS
        else 
            let f = pos |> ToFile
            let r = pos |> ToRank
            
            let nr, nf =
                match dir with
                | Dirn.N -> r + 1, f
                | Dirn.E -> r, f + 1
                | Dirn.S -> r - 1, f
                | Dirn.W -> r, f - 1
                | Dirn.NE -> r + 1, f + 1
                | Dirn.SE -> r - 1, f + 1
                | Dirn.SW -> r - 1, f - 1
                | Dirn.NW -> r + 1, f - 1
                | Dirn.NNE -> r + 2, f + 1
                | Dirn.EEN -> r + 1, f + 2
                | Dirn.EES -> r - 1, f + 2
                | Dirn.SSE -> r - 2, f + 1
                | Dirn.SSW -> r - 2, f - 1
                | Dirn.WWS -> r - 1, f - 2
                | Dirn.WWN -> r + 1, f - 2
                | Dirn.NNW -> r + 2, f - 1
                | _ -> Rank.EMPTY, File.EMPTY
            if nr = Rank.EMPTY && nf = File.EMPTY then OUTOFBOUNDS
            elif (nr |> Rank.IsInBounds) && (nf |> File.IsInBounds) then 
                Sq(nf, nr)
            else OUTOFBOUNDS
    
    let ToBitboard(pos : int) =
        if pos |> IsInBounds then (1UL <<< pos) 
        else 0UL
    
    let Between (pto : int) (pfrom : int) =
        let dir = pfrom |> DirectionTo(pto)
        
        let rec getb f rv =
            if f = pto then rv
            else 
                let nf = f |> PositionInDirectionUnsafe(dir)
                let nrv = rv ||| (nf |> ToBitboard)
                getb nf nrv
        
        let rv =
            if int (dir) = 0 then 0UL
            else getb pfrom 0UL
        
        rv &&& ~~~(pto |> ToBitboard)
