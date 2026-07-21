namespace Grampus

module MasterDataLogic =
    let formatCount (n: int) =
        if n >= 1_000_000 then sprintf "%.1fm" (float n / 1_000_000.0)
        elif n >= 1_000 then sprintf "%.1fk" (float n / 1_000.0)
        else n.ToString()
    type WinLossRatios = { WhitePct: float; DrawPct: float; BlackPct: float }
    let calculateRatios white draws black =
        let total = float (white + draws + black)
        if total = 0.0 then { WhitePct = 0.0; DrawPct = 0.0; BlackPct = 0.0 }
        else 
            { WhitePct = float white / total
              DrawPct = float draws / total
              BlackPct = float black / total }