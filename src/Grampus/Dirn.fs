namespace Grampus

module Dirn =
    let [<Literal>] N = 8
    let [<Literal>] E = 1
    let [<Literal>] S = -8
    let [<Literal>] W = -1
    let [<Literal>] NE = 9
    let [<Literal>] SE = -7
    let [<Literal>] SW = -9
    let [<Literal>] NW = 7
    let [<Literal>] NNE = 17
    let [<Literal>] EEN = 10
    let [<Literal>] EES = -6
    let [<Literal>] SSE = -15
    let [<Literal>] SSW = -17
    let [<Literal>] WWS = -10
    let [<Literal>] WWN = 6
    let [<Literal>] NNW = 15

    let AllDirectionsKnight =
        [| NNE; EEN; EES; SSE; SSW; WWS; WWN; NNW |]
    let AllDirectionsRook = [| N; E; S; W |]
    let AllDirectionsBishop =
        [| NE; SE; SW; NW |]
    let AllDirectionsQueen =
        [| N; E; S; W; NE; SE; SW; NW |]
    let Opposite(dir : int)  = -dir 
    let MyNorth(player : int) =
        if player = 0 then N
        else S
