namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus
open System.IO

module RepertoireTests =

    // Helper to create dummy moves for testing
    let m v = { From = v; To = v; Pc = v; CapPc = 0; Prom = 0 }
    let mv1 = m 1
    let mv2 = m 2
    let mv3 = m 3
    let mv4 = m 4

    [<Fact>]
    let ``getRequiredOrientation returns correct side`` () =
        let whiteRep = { Name = "White"; Side = WHITE; Lines = []; Comments = Map.empty }
        let blackRep = { Name = "Black"; Side = BLACK; Lines = []; Comments = Map.empty }
        
        Repertoire.getRequiredOrientation whiteRep |> should equal WHITE
        Repertoire.getRequiredOrientation blackRep |> should equal BLACK

    // --- UPDATE RULE 1 TESTS ---

    [<Fact>]
    let ``update - Rule 1: returns unchanged if path already exists as a prefix`` () =
        let initialLines = [ [mv1; mv2; mv3] ]
        let rep = { Name = "Test"; Side = WHITE; Lines = initialLines; Comments = Map.empty }
        
        // History [mv1], New Move [mv2]. [mv1; mv2] is a prefix of [mv1; mv2; mv3]
        let updated = Repertoire.update rep [mv1] mv2
        
        updated.Lines |> should equal initialLines

    // --- UPDATE RULE 2 TESTS (Opponent Side) ---

    [<Fact>]
    let ``update - Rule 2: Opponent move extends existing line if it matches history exactly`` () =
        // Repertoire for White. It's Black's turn (history length 1)
        let initialLines = [ [mv1] ] 
        let rep = { Name = "Test"; Side = WHITE; Lines = initialLines; Comments = Map.empty }
        
        let updated = Repertoire.update rep [mv1] mv2
        
        // Should transform [mv1] into [mv1; mv2]
        updated.Lines |> should contain [mv1; mv2]
        updated.Lines.Length |> should equal 1

    [<Fact>]
    let ``update - Rule 2: Opponent move adds new variation if history is not an exact line match`` () =
        // Repertoire for White. Black plays a variation at move 2
        let initialLines = [ [mv1; mv2] ] 
        let rep = { Name = "Test"; Side = WHITE; Lines = initialLines; Comments = Map.empty }
        
        // History is [mv1], new move is mv3 (Opponent variation)
        let updated = Repertoire.update rep [mv1] mv3
        
        updated.Lines |> should contain [mv1; mv2]
        updated.Lines |> should contain [mv1; mv3]
        updated.Lines.Length |> should equal 2

    // --- UPDATE RULE 3 TESTS (Our Side) ---

    [<Fact>]
    let ``update - Rule 3: Our side move replaces all existing variations at that point`` () =
        // Repertoire for White. White's turn (history length 0)
        let initialLines = [ [mv1; mv2]; [mv3; mv4] ]
        let rep = { Name = "Test"; Side = WHITE; Lines = initialLines; Comments = Map.empty }
        
        // White decides to play mv4 instead of mv1 or mv3 as the first move
        let updated = Repertoire.update rep [] mv4
        
        // All previous lines starting with [] (which is all of them) should be gone
        updated.Lines |> should equal [[mv4]]

    [<Fact>]
    let ``update - Rule 3: Our side move replaces deep variations and cleans comments`` () =
        let path1 = [mv1; mv2; mv3]
        let path2 = [mv1; mv2; mv4]
        let initialComments = Map.empty |> Map.add path1 "Old Comment"
        
        let rep = { Name = "Test"; Side = WHITE; Lines = [path1; path2]; Comments = initialComments }
        
        // It's White's turn (history length 2: [mv1; mv2]). 
        // White replaces mv3/mv4 with mv1
        let newMv = m 99
        let updated = Repertoire.update rep [mv1; mv2] newMv
        
        updated.Lines |> should equal [[mv1; mv2; newMv]]
        // Comment for the deleted path1 should be removed
        updated.Comments.IsEmpty |> should equal true

    // --- FILE IO TESTS ---

    [<Fact>]
    let ``load returns default repertoire when file is empty`` () =
        let side = WHITE
        let fileName = "repertoire_white.json"
        File.WriteAllText(fileName, "")
        try
            let loaded = Repertoire.load "" side
            loaded.Name |> should equal "New Repertoire"
        finally
            if File.Exists(fileName) then File.Delete(fileName)

    [<Fact>]
    let ``Saving repertoire creates a backup file`` () =
        let fol = "test_data_dir"
        if Directory.Exists(fol) then Directory.Delete(fol, true)
        Directory.CreateDirectory(fol) |> ignore
    
        try
            let rep = { Name = "Test"; Side = WHITE; Lines = []; Comments = Map.empty }
            Repertoire.save fol rep 
            System.Threading.Thread.Sleep(1100) 
            Repertoire.save fol rep 
    
            let backups = Repertoire.getVersions fol WHITE
            backups.Length |> should be (greaterThanOrEqualTo 1)
        finally
            if Directory.Exists(fol) then Directory.Delete(fol, true)