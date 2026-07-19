namespace Grampus

module Colour =
    /// Converts a Colour to its character representation ('w' or 'b').
    let toChar (c: int) =
        if c = WHITE then 'w' else 'b'
    /// Converts a character ('w' or 'b') to a Colour.
    let fromChar c =
        match c with
        | 'w' | 'W' -> WHITE
        | 'b' | 'B' -> BLACK
        | _ -> invalidArg "c" $"Invalid colour char: %c{c}"
    let FromStr c =
        match c with
        | "w" | "W" -> WHITE
        | "b" | "B" -> BLACK
        | _ -> invalidArg "c" $"Invalid colour string: %s{c}"
    let All = [| 0; 1 |]
    let Opp(col : int) = col ^^^ 1
    
