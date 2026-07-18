namespace Grampus

module File =

    let NAMES = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
    let EMPTY = 8
    let firstChar = int 'a'
    /// Converts a File to its character representation ('a'–'h').
    let toChar (f: int) : char =
        char (firstChar + f)
    let IsInBounds(file : int) = file >= 0 && file <= 7
    /// Converts a character ('a'–'h') to a File.
    let fromChar (c: char) : int =
        let ans = int c - firstChar
        if IsInBounds ans then ans
        else failwith (c.ToString() + " is not a valid file")
