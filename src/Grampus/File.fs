namespace Grampus

module File =
    let [<Literal>] A = 0
    let [<Literal>] B = 1
    let [<Literal>] C = 2
    let [<Literal>] D = 3
    let [<Literal>] E = 4
    let [<Literal>] F = 5
    let [<Literal>] G = 6
    let [<Literal>] H = 7

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
