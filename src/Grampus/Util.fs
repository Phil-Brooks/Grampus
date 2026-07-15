namespace Grampus

[<AutoOpen>]
module Util =
    let PcTp i = enum<PieceType> (i)
    let Pc i = enum<Piece> (i)
    let BitB i =
        Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint64, Bitboard>
            (i)
    let (-!) (r : Rank) (i : int) : Rank = r - i
    let (+!) (r : Rank) (i : int) : Rank = r + i
    let (--) (f : File) (i : int) : File = f - i
    let (++) (f : File) (i : int) : File = f + i
    
   
