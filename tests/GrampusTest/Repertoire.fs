namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus
open System.IO

module RepertoireTests =

    // --- Helpers for cleaner tests ---
    
    let createTestMv fromSq toSq pc = 
        { From = fromSq; To = toSq; Pc = pc; CapPc = 0; Prom = 0 }
    let createTestMvP fromSq toSq pc prom = 
        { From = fromSq; To = toSq; Pc = pc; CapPc = 0; Prom = prom }


    // --- Setup a test tree with Enum syntax ---
    let nodeNf3 = { Mv = createTestMv G1 F3 2; San = "Nf3"; Comment = ""; Replies = [] }
    let nodeE5  = { Mv = createTestMv E7 E5 9; San = "e5"; Comment = ""; Replies = [nodeNf3] }
    let nodeC5  = { Mv = createTestMv C7 C5 9; San = "c5"; Comment = ""; Replies = [] }
    let nodeE4  = { Mv = createTestMv E2 E4 1; San = "e4"; Comment = ""; Replies = [nodeE5; nodeC5] }
    let nodeD4  = { Mv = createTestMv D2 D4 1; San = "d4"; Comment = ""; Replies = [] }
    
    let testRoots = [nodeE4; nodeD4]
    let testRepertoire = { Name = "Test Rep"; Side = 0; Roots = testRoots }

    // --- 1. Orientation Tests ---

    [<Fact>]
    let ``getRequiredOrientation returns correct side`` () =
        let whiteRep = { Name = "White"; Side = 0; Roots = [] }
        let blackRep = { Name = "Black"; Side = 1; Roots = [] }
        
        Repertoire.getRequiredOrientation whiteRep |> should equal 0
        Repertoire.getRequiredOrientation blackRep |> should equal 1

    // --- 2. Branch Finding Tests ---

    [<Fact>]
    let ``findCurrentBranch returns roots when moves list is empty`` () =
        Repertoire.findCurrentBranch testRoots [] |> should equal (Some testRoots)

    [<Fact>]
    let ``findCurrentBranch finds replies after first move`` () =
        let moves = [ createTestMv E2 E4 1 ]
        match Repertoire.findCurrentBranch testRoots moves with
        | Some replies -> 
            replies.Length |> should equal 2
            replies.[0].San |> should equal "e5"
        | None -> failwith "Should have found branch"

    [<Fact>]
    let ``findCurrentBranch handles deep paths`` () =
        let moves = [ createTestMv E2 E4 1; createTestMv E7 E5 9 ]
        let result = Repertoire.findCurrentBranch testRoots moves
        result.Value.[0].San |> should equal "Nf3"

    // --- 3. Serialization Tests (NEW) ---

    [<Fact>]
    let ``Save and Load preserves data integrity`` () =
        let testFile = "repertoire_white.json"
        if File.Exists(testFile) then File.Delete(testFile)

        try
            // Save
            Repertoire.save testRepertoire
            File.Exists(testFile) |> should be True

            // Load
            let loaded = Repertoire.load 0
            loaded.Name |> should equal "Test Rep"
            loaded.Roots.Length |> should equal 2
            loaded.Roots.[0].San |> should equal "e4"
        finally
            if File.Exists(testFile) then File.Delete(testFile)

    [<Fact>]
    let ``Load returns empty repertoire if file missing`` () =
        let nonExistentSide = 99
        let result = Repertoire.load nonExistentSide
        result.Roots |> should be Empty
        result.Side |> should equal nonExistentSide

    // --- 4. Helper Function Tests ---

    [<Fact>]
    let ``createNode initializes correctly`` () =
        let mv = createTestMv E2 E4 1
        let node = Repertoire.createNode mv "e4" 
        
        node.Mv.From |> should equal E2
        node.Replies |> should be Empty

    [<Fact>]
    let ``findCurrentBranch distinguishes between different promotion pieces`` () =
        // Setup: Two branches from the same square, one promoting to Queen, one to Rook
        let moveToQueen = createTestMvP A7 A8 WPAWN WQUEEN
        let moveToRook  = createTestMvP A7 A8 WPAWN WROOK
        
        let nodeQueen = { Mv = moveToQueen; San = "a8=Q"; Comment = "Best"; Replies = [] }
        let nodeRook  = { Mv = moveToRook; San = "a8=R"; Comment = "Silly"; Replies = [] }
        
        let roots = [nodeQueen; nodeRook]

        // Test 1: Looking for Queen promotion
        let searchQueen = [ createTestMvP A7 A8 WPAWN WQUEEN ]
        let resultQueen = Repertoire.findCurrentBranch roots searchQueen
        resultQueen.Value |> should be Empty // Found the node, returns its (empty) replies
        
        // Test 2: Looking for Rook promotion
        let searchRook = [ createTestMvP A7 A8 WPAWN WROOK ]
        let resultRook = Repertoire.findCurrentBranch roots searchRook
        resultRook.IsSome |> should be True

        // Test 3: Looking for No promotion (should fail)
        let searchNoProm = [ createTestMvP A7 A8 WPAWN 0 ]
        let resultNone = Repertoire.findCurrentBranch roots searchNoProm
        resultNone |> should equal None

    // --- 2. Robustness / Invalid JSON Tests ---

    [<Fact>]
    let ``load returns default repertoire when file contains invalid JSON`` () =
        let side = WHITE
        let fileName = "repertoire_white.json"
        
        // Write garbage to the file
        File.WriteAllText(fileName, "{ \"this is\": not valid json [ }")

        try
            let loaded = Repertoire.load side
            
            // Should not crash, should return default
            loaded.Name |> should equal "New Repertoire"
            loaded.Side |> should equal side
            loaded.Roots |> should be Empty
        finally
            // Cleanup
            if File.Exists(fileName) then File.Delete(fileName)

    [<Fact>]
    let ``load returns default repertoire when file is empty`` () =
        let side = WHITE
        let fileName = "repertoire_white.json"
        
        File.WriteAllText(fileName, "")

        try
            let loaded = Repertoire.load side
            loaded.Name |> should equal "New Repertoire"
        finally
            if File.Exists(fileName) then File.Delete(fileName)

    // --- 3. Deep Path Test (Edge Case) ---

    [<Fact>]
    let ``findCurrentBranch returns None if path starts correctly but diverges later`` () =
        let move1 = createTestMvP 12 28 1 0 // e4
        let move2 = createTestMvP 52 36 9 0 // e5
        let move3_actual = createTestMvP 50 34 9 0 // c5 (The move actually played)
        
        let nodeE5 = { Mv = move2; San = "e5"; Comment = ""; Replies = [] }
        let nodeE4 = { Mv = move1; San = "e4"; Comment = ""; Replies = [nodeE5] }
        
        // Played e4, then c5. But the book only knows e4 followed by e5.
        let playedMoves = [ move1; move3_actual ]
        
        let result = Repertoire.findCurrentBranch [nodeE4] playedMoves
        result |> should equal None