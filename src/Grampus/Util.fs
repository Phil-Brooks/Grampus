namespace Grampus

[<AutoOpen>]
module Util =
    let Pc i = enum<Piece> (i)
    let BitB i =
        Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint64, Bitboard>
            (i)
    
   
