namespace Grampus

[<AutoOpen>]
module Util =
    let BitB i =
        Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint64, Bitboard>
            (i)
    
   
