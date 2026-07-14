namespace GrampusInternal

open Grampus

[<AutoOpen>]
module Util =
    let PcTp i = enum<PieceType> (i)
    let Pc i = enum<Piece> (i)
    let Plyr i = enum<Player> (i)
    let BitB i =
        Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint64, Bitboard>
            (i)
    let (-!) (r : Rank) (i : int16) : Rank = r - i
    let (+!) (r : Rank) (i : int16) : Rank = r + i
    let (--) (f : File) (i : int16) : File = f - i
    let (++) (f : File) (i : int16) : File = f + i
    
   
