namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus
open System.IO

module Repertoire =

    let createTestMv fromSq toSq pc = 
        { From = fromSq; To = toSq; Pc = pc; CapPc = 0; Prom = 0 }
    let createTestMvP fromSq toSq pc prom = 
        { From = fromSq; To = toSq; Pc = pc; CapPc = 0; Prom = prom }

    [<Fact>]
    let ``getRequiredOrientation returns correct side`` () =
        let whiteRep = { Name = "White"; Side = 0; Lines = []; Comments = Map.empty }
        let blackRep = { Name = "Black"; Side = 1; Lines = []; Comments = Map.empty }
        
        Repertoire.getRequiredOrientation whiteRep |> should equal 0
        Repertoire.getRequiredOrientation blackRep |> should equal 1

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
        let fol = "test_data"
        if Directory.Exists(fol) then Directory.Delete(fol, true)
        Directory.CreateDirectory(fol) |> ignore
    
        let rep = { Name = "Test"; Side = WHITE; Lines = []; Comments = Map.empty }
        Repertoire.save fol rep // First save
        System.Threading.Thread.Sleep(1100) // Ensure timestamp differs
        Repertoire.save fol rep // Second save (should trigger backup)
    
        let backups = Repertoire.getVersions fol WHITE
        backups.Length |> should equal 1