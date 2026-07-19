namespace Grampus

module Square =
    let Parse(s : string) =
        if s.Length <> 2 then failwith (s + " is not a valid position")
        else 
            let file = File.fromChar(s.[0])
            let rank = Rank.fromChar(s.[1])
            SQ(file, rank)
    let ToStr(sq :int) =
        if sq = OUTOFBOUNDS then "-"
        else
            let f = FL sq
            let r = RNK sq
            (f |> File.ToStr) + (r |> Rank.ToStr)
    let InBounds(sq : int) = sq >= 0 && sq <= 63
    let DirectionTo (pto : int) (pfrom : int) =
        let rankfrom = int (pfrom |> RNK)
        let filefrom = int (pfrom |> FL)
        let rankto = int (pto |> RNK)
        let fileto = int (pto |> FL)
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
    let InDirn (dir : int) (sq : int) : int =
        sq + dir
    
