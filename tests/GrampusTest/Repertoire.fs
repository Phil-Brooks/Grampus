namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus
open System.IO

module Repertoire =

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

    // --- 5. Repertoire Update Logic Tests ---

    [<Fact>]
    let ``update OUR SIDE: adds move to empty roots`` () =
        let rep = { Name = "White Study"; Side = WHITE; Roots = [] }
        let moveE4 = createTestMv E2 E4 1
        
        let updated = Repertoire.update rep [] moveE4 "e4"
        
        updated.Roots.Length |> should equal 1
        updated.Roots.[0].San |> should equal "e4"

    [<Fact>]
    let ``update OUR SIDE: replaces existing move with new one (Single Path Rule)`` () =
        // Initial state: We have d4 in our repertoire
        let moveD4 = createTestMv D2 D4 1
        let moveE4 = createTestMv E2 E4 1
        let nodeD4 = { Mv = moveD4; San = "d4"; Comment = "Old"; Replies = [] }
        let rep = { Name = "White Study"; Side = WHITE; Roots = [nodeD4] }

        // Update: We play e4 instead
        let updated = Repertoire.update rep [] moveE4 "e4"

        // Result: d4 should be gone, replaced by e4
        updated.Roots.Length |> should equal 1
        updated.Roots.[0].San |> should equal "e4"

    [<Fact>]
    let ``update OUR SIDE: keeps existing move and branches if move is the same`` () =
        let moveE4 = createTestMv E2 E4 1
        let reply = { Mv = createTestMv E7 E5 9; San = "e5"; Comment = ""; Replies = [] }
        let nodeE4 = { Mv = moveE4; San = "e4"; Comment = "Keep me"; Replies = [reply] }
        let rep = { Name = "White Study"; Side = WHITE; Roots = [nodeE4] }

        // Update with same move
        let updated = Repertoire.update rep [] moveE4 "e4"

        updated.Roots.Length |> should equal 1
        updated.Roots.[0].Comment |> should equal "Keep me"
        updated.Roots.[0].Replies.Length |> should equal 1 // Branch preserved

    [<Fact>]
    let ``update OPPONENT SIDE: adds variation instead of replacing`` () =
        // Repertoire for WHITE. History: 1. e4. Opponent (Black) plays.
        let moveE4 = createTestMv E2 E4 1
        let moveE5 = createTestMv E7 E5 9
        let moveC5 = createTestMv C7 C5 9
        
        let nodeE5 = { Mv = moveE5; San = "e5"; Comment = ""; Replies = [] }
        let nodeE4 = { Mv = moveE4; San = "e4"; Comment = ""; Replies = [nodeE5] }
        let rep = { Name = "White Study"; Side = WHITE; Roots = [nodeE4] }

        // Black plays c5 (Sicilian) instead of e5
        let updated = Repertoire.update rep [moveE4] moveC5 "c5"

        // Result: e4 should now have TWO replies (e5 AND c5)
        let root = updated.Roots.[0]
        root.Replies.Length |> should equal 2
        root.Replies |> List.exists (fun n -> n.San = "e5") |> should be True
        root.Replies |> List.exists (fun n -> n.San = "c5") |> should be True

    [<Fact>]
    let ``update Deep Path: correctly navigates and updates several moves deep`` () =
        // 1. e4 e5 2. Nf3 ... -> add 2... Nc6
        let m1 = createTestMv E2 E4 1   // White
        let m2 = createTestMv E7 E5 9   // Black
        let m3 = createTestMv G1 F3 2   // White
        let m4 = createTestMv B8 C6 10  // Black (New Variation)

        let nodeNf3 = { Mv = m3; San = "Nf3"; Comment = ""; Replies = [] }
        let nodeE5 = { Mv = m2; San = "e5"; Comment = ""; Replies = [nodeNf3] }
        let nodeE4 = { Mv = m1; San = "e4"; Comment = ""; Replies = [nodeE5] }
        let rep = { Name = "White Study"; Side = WHITE; Roots = [nodeE4] }

        let history = [m1; m2; m3]
        let updated = Repertoire.update rep history m4 "Nc6"

        // Verify structure: e4 -> e5 -> Nf3 -> Nc6
        let branch = Repertoire.findCurrentBranch updated.Roots [m1; m2; m3]
        branch.Value.Length |> should equal 1
        branch.Value.[0].San |> should equal "Nc6"

    [<Fact>]
    let ``update Black Repertoire: applies single path rule to Black moves`` () =
        // Studying BLACK. 1. e4 has been played. 
        // We currently have 1... e5 in rep. We want to change to 1... c5.
        let m1 = createTestMv E2 E4 1
        let mE5 = createTestMv E7 E5 9
        let mC5 = createTestMv C7 C5 9
        
        let nodeE5 = { Mv = mE5; San = "e5"; Comment = ""; Replies = [] }
        let nodeE4 = { Mv = m1; San = "e4"; Comment = ""; Replies = [nodeE5] }
        let rep = { Name = "Black Study"; Side = BLACK; Roots = [nodeE4] }

        // Update history is [e4]. Next turn is Black (Our side).
        let updated = Repertoire.update rep [m1] mC5 "c5"

        // Result: e5 should be REPLACED by c5 because it's our side
        let repliesToE4 = updated.Roots.[0].Replies
        repliesToE4.Length |> should equal 1
        repliesToE4.[0].San |> should equal "c5"