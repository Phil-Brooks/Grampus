namespace Grampus

module Rank =
    let List = [ R1; R2; R3; R4; R5; R6; R7; R8 ]
    let firstChar = int '1'
    /// Converts a Rank to its character representation ('1'–'8').
    let toChar (r: int) : char =
        char (firstChar + int r)
    let IsInBounds(rank : int) = rank >= 0 && rank <= 7
    /// Converts a character ('1'–'8') to a Rank.
    let fromChar (c: char) : int =
        let ans = int c - firstChar
        if IsInBounds ans then ans
        else failwith (c.ToString() + " is not a valid rank")
    let ToStr(rank : int) = toChar(rank).ToString()
    let MyRanks =
        [| [| R1; R2; R3; R4; R5; R6; R7; R8 |]
           [| R8; R7; R6; R5; R4; R3; R2; R1 |] |]
    let MyRank (rank : int) (colour : int) =
        MyRanks.[colour].[rank]
