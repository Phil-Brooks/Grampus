namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus

module Repertoire =

    // --- Helper to create a basic move for testing ---
    let createTestMv fromSq toSq = 
        { From = fromSq; To = toSq; Pc = 0; CapPc = 0; Prom = 0 }

    // --- Setup a small test tree ---
    // e4 -> e5 -> Nf3
    //    -> c5
    // d4
    let nodeNf3 = { Mv = createTestMv G1 F3; San = "Nf3"; Annotation = MainLine; Comment = ""; Replies = [] }
    let nodeE5  = { Mv = createTestMv E7 E5; San = "e5"; Annotation = Opponent; Comment = ""; Replies = [nodeNf3] }
    let nodeC5  = { Mv = createTestMv C7 C5; San = "c5"; Annotation = Opponent; Comment = ""; Replies = [] }
    let nodeE4  = { Mv = createTestMv E2 E4; San = "e4"; Annotation = MainLine; Comment = ""; Replies = [nodeE5; nodeC5] }
    let nodeD4  = { Mv = createTestMv D2 D4; San = "d4"; Annotation = Alternative; Comment = ""; Replies = [] }
    
    let testRoots = [nodeE4; nodeD4]

    // --- 1. Orientation Tests ---

    [<Fact>]
    let ``getRequiredOrientation returns correct side`` () =
        let whiteRep = { Name = "White Repertoire"; Side = WHITE; Roots = [] }
        let blackRep = { Name = "Black Repertoire"; Side = BLACK; Roots = [] }
        
        Repertoire.getRequiredOrientation whiteRep |> should equal WHITE
        Repertoire.getRequiredOrientation blackRep |> should equal BLACK

    // --- 2. Branch Finding Tests ---

    [<Fact>]
    let ``findCurrentBranch returns roots when moves list is empty`` () =
        let result = Repertoire.findCurrentBranch testRoots []
        result |> should equal (Some testRoots)

    [<Fact>]
    let ``findCurrentBranch finds replies after first move`` () =
        let moves = [ createTestMv E2 E4 ]
        let result = Repertoire.findCurrentBranch testRoots moves
        
        result.IsSome |> should be True
        let replies = result.Value
        replies.Length |> should equal 2
        replies.[0].San |> should equal "e5"
        replies.[1].San |> should equal "c5"

    [<Fact>]
    let ``findCurrentBranch finds deep branch correctly`` () =
        let moves = [ createTestMv E2 E4; createTestMv E7 E5 ]
        let result = Repertoire.findCurrentBranch testRoots moves
        
        result.IsSome |> should be True
        result.Value.Length |> should equal 1
        result.Value.[0].San |> should equal "Nf3"

    [<Fact>]
    let ``findCurrentBranch returns Some empty list when at end of book`` () =
        let moves = [ createTestMv E2 E4; createTestMv E7 E5; createTestMv G1 F3 ]
        let result = Repertoire.findCurrentBranch testRoots moves
        
        result.IsSome |> should be True
        result.Value |> should be Empty

    [<Fact>]
    let ``findCurrentBranch returns None when move is not in book`` () =
        let moves = [ createTestMv A2 A3 ] // Irregular move
        let result = Repertoire.findCurrentBranch testRoots moves
        
        result |> should equal None

    [<Fact>]
    let ``findCurrentBranch returns None when deep move diverges from book`` () =
        // e4 is in book, but then g6 is not a reply to e4 in our test tree
        let moves = [ createTestMv E2 E4; createTestMv G7 G6 ]
        let result = Repertoire.findCurrentBranch testRoots moves
        
        result |> should equal None

    // --- 3. Helper Function Tests ---

    [<Fact>]
    let ``createNode initializes node with empty replies and comment`` () =
        let mv = createTestMv E2 E4
        let node = Repertoire.createNode mv "e4" MainLine
        
        node.Mv |> should equal mv
        node.San |> should equal "e4"
        node.Annotation |> should equal MainLine
        node.Replies |> should be Empty
        node.Comment |> should equal ""