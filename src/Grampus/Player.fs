namespace Grampus

module Player =
    let [<Literal>] White = 0
    let [<Literal>] Black = 1

    /// Converts a Colour to its character representation ('w' or 'b').
    let toChar (c: int) =
        if c = White then 'w' else 'b'

    /// Converts a character ('w' or 'b') to a Colour.
    let fromChar c =
        match c with
        | 'w' | 'W' -> White
        | 'b' | 'B' -> Black
        | _ -> invalidArg "c" $"Invalid colour char: %c{c}"
    
    let AllPlayers = [| 0; 1 |]
    let PlayerOther(player : int) = (player ^^^ 1)
    
    let MyRanks =
        [| [| Rank1; Rank2; Rank3; Rank4; Rank5; Rank6; Rank7; Rank8 |]
           [| Rank8; Rank7; Rank6; Rank5; Rank4; Rank3; Rank2; Rank1 |] |]
    
    let MyRank (rank : Rank) (player : int) =
        MyRanks.[int (player)].[int (rank)]
