namespace Grampus

module Rank =
    let [<Literal>] R1 = 0
    let [<Literal>] R2 = 1
    let [<Literal>] R3 = 2
    let [<Literal>] R4 = 3
    let [<Literal>] R5 = 4
    let [<Literal>] R6 = 5
    let [<Literal>] R7 = 6
    let [<Literal>] R8 = 7

    // Must NOT be private if inline functions use it
    let firstChar = int '1'

    /// Converts a Rank to its character representation ('1'–'8').
    let toChar (r: int) : char =
        char (firstChar + int r)

    let IsInBounds(rank : Rank) = rank >= 0 && rank <= 7
    /// Converts a character ('1'–'8') to a Rank.
    let fromChar (c: char) : int =
        let ans = int c - firstChar
        if IsInBounds ans then ans
        else failwith (c.ToString() + " is not a valid rank")

    let RankToString(rank : Rank) = RANK_NAMES.[int (rank)]
    
    let ToBitboard(rank : Rank) =
        if rank = Rank1 then Bitboard.Rank1
        elif rank = Rank2 then Bitboard.Rank2
        elif rank = Rank3 then Bitboard.Rank3
        elif rank = Rank4 then Bitboard.Rank4
        elif rank = Rank5 then Bitboard.Rank5
        elif rank = Rank6 then Bitboard.Rank6
        elif rank = Rank7 then Bitboard.Rank7
        elif rank = Rank8 then Bitboard.Rank8
        else Bitboard.Empty
