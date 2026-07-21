namespace Grampus

module Castle =
    let EMPTY =
        {
            WK = false
            WQ = false
            BK = false
            BQ = false
        }
    let FromStr (s: string) =
        if s = "-" then EMPTY
        else
            {
                WK = s.Contains "K"
                WQ = s.Contains "Q"
                BK = s.Contains "k"
                BQ = s.Contains "q"
            }
    let ToStr (rights: Cstl) =
        if rights = EMPTY then "-"
        else
            let k = if rights.WK then "K" else ""
            let q = if rights.WQ then "Q" else ""
            let bk = if rights.BK then "k" else ""
            let bq = if rights.BQ then "q" else ""
            k + q + bk + bq
