namespace Grampus

open System.Numerics

module Bitboard =
    
    let [<Literal>] Rank8 = 18374686479671623680UL
    let [<Literal>] Rank7 = 71776119061217280UL
    let [<Literal>] Rank6 = 280375465082880UL
    let [<Literal>] Rank5 = 1095216660480UL
    let [<Literal>] Rank4 = 4278190080UL
    let [<Literal>] Rank3 = 16711680UL
    let [<Literal>] Rank2 = 65280UL
    let [<Literal>] Rank1 = 255UL
    let [<Literal>] FileA = 72340172838076673UL
    let [<Literal>] FileB = 144680345676153346UL
    let [<Literal>] FileC = 289360691352306692UL
    let [<Literal>] FileD = 578721382704613384UL
    let [<Literal>] FileE = 1157442765409226768UL
    let [<Literal>] FileF = 2314885530818453536UL
    let [<Literal>] FileG = 4629771061636907072UL
    let [<Literal>] FileH = 9259542123273814144UL
    let [<Literal>] Empty = 0UL
    let [<Literal>] Full = 18446744073709551615UL
    
    let bitCount (ibb: uint64) =
        BitOperations.PopCount(ibb)

    let getFirstPos (ibb: uint64) : int =
        if ibb = Empty then OUTOFBOUNDS
        else BitOperations.TrailingZeroCount(ibb)

    /// Removes the first bit and returns (Square, RemainingBitboard)
    let popFirst (ibb: uint64) =
        let first = getFirstPos ibb
        // This bitwise trick clears the lowest set bit
        let remaining = uint64 ibb &&& (uint64 ibb - 1UL)
        first, remaining
    
    let inline shiftN (ibb: uint64) = (uint64 (ibb &&& ~~~Rank8) <<< 8) 
    let inline shiftS (ibb: uint64) = (uint64 (ibb &&& ~~~Rank1) >>> 8) 
    let inline shiftE (ibb: uint64) = (uint64 (ibb &&& ~~~FileH) <<< 1) 
    let inline shiftW (ibb: uint64) = (uint64 (ibb &&& ~~~FileA) >>> 1) 

    let shift dir (ibb: uint64) =
        match dir with
        | Dirn.N -> shiftN ibb
        | Dirn.S -> shiftS ibb
        | Dirn.E -> shiftE ibb
        | Dirn.W -> shiftW ibb
        | Dirn.NE -> shiftN (shiftE ibb)
        | Dirn.NW -> shiftN (shiftW ibb)
        | Dirn.SE -> shiftS (shiftE ibb)
        | Dirn.SW -> shiftS (shiftW ibb)
        // Knight moves...
        | Dirn.NNE -> shiftN (shiftN (shiftE ibb))
        | Dirn.EEN -> shiftE (shiftE (shiftN ibb))
        | Dirn.EES -> shiftE (shiftE (shiftS ibb))
        | Dirn.SSE -> shiftS (shiftS (shiftE ibb))
        | Dirn.SSW -> shiftS (shiftS (shiftW ibb))
        | Dirn.WWS -> shiftW (shiftW (shiftS ibb))
        | Dirn.WWN -> shiftW (shiftW (shiftN ibb))
        | Dirn.NNW -> shiftN (shiftN (shiftW ibb))
        | _ -> failwith "invalid dir"

    let flood dir (ibb : uint64) =
        let rec loop (current : uint64) =
            let next = current ||| (current |> shift dir)
            if next = current then current
            else loop next
        loop ibb

    let containsPos (pos : int) (ibb : uint64) =
        let mask = 1UL <<< pos
        (ibb &&& mask) <> 0UL

    let toSquares (ibb: uint64) =
        [| 
            let mutable temp = ibb
            while temp <> 0UL do
                let lsb = BitOperations.TrailingZeroCount(temp)
                yield int lsb // Returns Square (int16)
                temp <- temp &&& (temp - 1UL)
        |]