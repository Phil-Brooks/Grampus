namespace GrampusInternal

open Grampus

module MoveUtil =
    ///Get an encoded move from a SAN Move(move) such as Nf3 for this Board(bd)
    let fromSAN (bd : Brd) (move : string) =
        let pmv = move |> pMove.Parse
        let mv = pmv |> pMove.ToMove bd
        mv
    
