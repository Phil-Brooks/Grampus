namespace Grampus

module Castle =
    let [<Literal>] EMPTY = 0
    let [<Literal>] WK   = 1
    let [<Literal>] WQ   = 2
    let [<Literal>] BK   = 4
    let [<Literal>] BQ   = 8

    let [<Literal>] White = 3 // (WK | WQ)
    let [<Literal>] Black = 12 // (BK | BQ)
    let [<Literal>] All   = 15

    let fromString (s: string) =
        if s = "-" then EMPTY
        else
            (if s.Contains "K" then WK else EMPTY)
            ||| (if s.Contains "Q" then WQ else EMPTY)
            ||| (if s.Contains "k" then BK else EMPTY)
            ||| (if s.Contains "q" then BQ else EMPTY)

    let toString (rights: int) =
        if rights = EMPTY then "-"
        else
            let k = if rights &&& WK <> 0 then "K" else ""
            let q = if rights &&& WQ <> 0 then "Q" else ""
            let bk = if rights &&& BK <> 0 then "k" else ""
            let bq = if rights &&& BQ <> 0 then "q" else ""
            k + q + bk + bq
