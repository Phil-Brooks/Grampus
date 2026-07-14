namespace Grampus

open System.Numerics

module Bitboard =
    // Helper to cast raw uint64 back to your Bitboard Enum
    let inline private bb (v: uint64) = LanguagePrimitives.EnumOfValue<uint64, Bitboard>(v)

    let bitCount (ibb: Bitboard) =
        BitOperations.PopCount(uint64 ibb)

    let getFirstPos (ibb: Bitboard) : Square =
        if ibb = Bitboard.Empty then OUTOFBOUNDS
        else int16 (BitOperations.TrailingZeroCount(uint64 ibb))

    /// Removes the first bit and returns (Square, RemainingBitboard)
    let popFirst (ibb: Bitboard) =
        let first = getFirstPos ibb
        // This bitwise trick clears the lowest set bit
        let remaining = uint64 ibb &&& (uint64 ibb - 1UL)
        first, bb remaining
    
    let inline shiftN (ibb: Bitboard) = (uint64 (ibb &&& ~~~Bitboard.Rank8) <<< 8) |> bb
    let inline shiftS (ibb: Bitboard) = (uint64 (ibb &&& ~~~Bitboard.Rank1) >>> 8) |> bb
    let inline shiftE (ibb: Bitboard) = (uint64 (ibb &&& ~~~Bitboard.FileH) >>> 1) |> bb
    let inline shiftW (ibb: Bitboard) = (uint64 (ibb &&& ~~~Bitboard.FileA) <<< 1) |> bb

    let shift dir (ibb: Bitboard) =
        match dir with
        | Dirn.DirN -> shiftN ibb
        | Dirn.DirS -> shiftS ibb
        | Dirn.DirE -> shiftE ibb
        | Dirn.DirW -> shiftW ibb
        | Dirn.DirNE -> shiftN (shiftE ibb)
        | Dirn.DirNW -> shiftN (shiftW ibb)
        | Dirn.DirSE -> shiftS (shiftE ibb)
        | Dirn.DirSW -> shiftS (shiftW ibb)
        // Knight moves...
        | Dirn.DirNNE -> shiftN (shiftN (shiftE ibb))
        | Dirn.DirEEN -> shiftE (shiftE (shiftN ibb))
        | Dirn.DirEES -> shiftE (shiftE (shiftS ibb))
        | Dirn.DirSSE -> shiftS (shiftS (shiftE ibb))
        | Dirn.DirSSW -> shiftS (shiftS (shiftW ibb))
        | Dirn.DirWWS -> shiftW (shiftW (shiftS ibb))
        | Dirn.DirWWN -> shiftW (shiftW (shiftN ibb))
        | Dirn.DirNNW -> shiftN (shiftN (shiftW ibb))
        | _ -> failwith "invalid dir"

    let flood dir (ibb : Bitboard) =
        let rec loop (current : Bitboard) =
            let next = current ||| (current |> shift dir)
            if next = current then current
            else loop next
        loop ibb

    let containsPos (pos : Square) (ibb : Bitboard) =
        let mask = 1UL <<< int pos
        (uint64 ibb &&& mask) <> 0UL

    let toSquares (ibb: Bitboard) =
        [| 
            let mutable temp = uint64 ibb
            while temp <> 0UL do
                let lsb = BitOperations.TrailingZeroCount(temp)
                yield int16 lsb // Returns Square (int16)
                temp <- temp &&& (temp - 1UL)
        |]