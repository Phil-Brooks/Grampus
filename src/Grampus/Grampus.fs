namespace Grampus


/// <summary>
/// Holds the functions related to a Piece.
/// </summary>
module Piece =
    /// <summary>
    /// Gets the string symbol for a Piece.
    /// </summary>
    /// <param name="piece">The piece, such as Piece.WKnight.</param>
    /// <returns>The result as a string, such as N.</returns>
    let ToStr(piece) = GrampusInternal.Piece.PieceToString(piece)

/// <summary>
/// Holds the functions related to a Board.
/// </summary>
module Board =
    
    ///The starting Board at the beginning of a game
    let Start = GrampusInternal.Board.Start
    
    /// <summary>
    /// Gets all possible moves for this Board from the specified Square.
    /// </summary>
    /// <param name="bd">The Board as a Brd type.</param>
    /// <param name="sq">The Square as a Square type.</param>
    /// <returns>The list of all possible moves.</returns>
    let PossMoves bd sq = GrampusInternal.MoveGenerate.PossMoves bd sq
    
    /// <summary>
    /// Make an encoded Move for this Board and return the new Board.
    /// </summary>
    /// <param name="mv">The move as a Move type.</param>
    /// <param name="bd">The Board as a Brd type.</param>
    /// <returns>The new Board as a Brd type.</returns>
    let Push mv bd = GrampusInternal.Board.MoveApply mv bd

/// <summary>
/// Holds the functions related to a Move.
/// </summary>
module Move =
    /// <summary>
    /// Get the source Square for an encoded Move.
    /// </summary>
    /// <param name="mv">The move as a Move type.</param>
    /// <returns>The source square.</returns>
    let From(mv) = GrampusInternal.Move.From(mv)
    
    /// <summary>
    /// Get the target Square for an encoded Move.
    /// </summary>
    /// <param name="mv">The move as a Move type.</param>
    /// <returns>The target square.</returns>
    let To(mv) = GrampusInternal.Move.To(mv)
    
    /// <summary>
    /// Get the promoted PieceType for an encoded Move.
    /// </summary>
    /// <param name="mv">The move as a Move type.</param>
    /// <returns>The promoted piece type as a PieceType type.</returns>
    let PromPcTp(mv) = GrampusInternal.Move.PromoteType(mv)
    
    /// <summary>
    /// Get an encoded move from a SAN string such as Nf3 for this Board.
    /// </summary>
    /// <param name="bd">The Board as a Brd type.</param>
    /// <param name="san">The SAN string, such as Nf3.</param>
    /// <returns>The move as a Move type.</returns>
    let FromSan bd san = GrampusInternal.MoveUtil.fromSAN bd san
