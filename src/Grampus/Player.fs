namespace Grampus

module Player =
    let AllPlayers = [| 0; 1 |]
    let PlayerOther(player : int) = (player ^^^ 1)
    
    let MyRanks =
        [| [| Rank1; Rank2; Rank3; Rank4; Rank5; Rank6; Rank7; Rank8 |]
           [| Rank8; Rank7; Rank6; Rank5; Rank4; Rank3; Rank2; Rank1 |] |]
    
    let MyRank (rank : Rank) (player : int) =
        MyRanks.[int (player)].[int (rank)]
