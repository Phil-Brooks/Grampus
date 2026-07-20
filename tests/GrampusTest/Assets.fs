namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open GrampusUI // Ensure this is accessible
open System.Drawing

module Assets =

    [<Fact>]
    let ``Ensure all piece assets can be loaded and cursors created`` () =
        // This will trigger the static initialization of the Assets module
        // If a resource is missing, Assets.loadPiece will throw a failwith
        let pieceKeys = Assets.Pieces.Keys |> Seq.toList
        pieceKeys.Length |> should equal 12
        
        let cursorKeys = Assets.Cursors.Keys |> Seq.toList
        cursorKeys.Length |> should equal 12

    [<Fact>]
    let ``Ensure main UI icons exist`` () =
        // Accessing these properties will throw if the streams are null
        Assets.Back |> should not' (equal null)
        Assets.Orient |> should not' (equal null)
        Assets.Grampus |> should not' (equal null)