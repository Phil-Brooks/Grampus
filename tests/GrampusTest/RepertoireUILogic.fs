namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus
open System.Drawing

module RepertoireUILogic =

    // --- 1. Testing getAnnotationDetails ---
    
    // Define the test data as a static member
    let annotationCases : obj[][] = [|
        [| MainLine; "Main"; FontStyle.Bold |]
        [| Alternative; "Alt"; FontStyle.Regular |]
        [| Opponent; "Opp"; FontStyle.Italic |]
    |]

    [<Theory>]
    [<MemberData(nameof(annotationCases))>]
    let ``getAnnotationDetails returns correct text and style`` (annot: MoveAnnotation) (expectedText: string) (expectedStyle: FontStyle) =
        let text, _, style = RepertoireUILogic.getAnnotationDetails annot
        
        text |> should equal expectedText
        style |> should equal expectedStyle
    
    [<Fact>]
    let ``getAnnotationDetails returns specific colors for specific types`` () =
        let _, mainCol, _ = RepertoireUILogic.getAnnotationDetails MainLine
        let _, altCol, _ = RepertoireUILogic.getAnnotationDetails Alternative
        
        mainCol |> should equal Color.DarkGreen
        altCol |> should equal Color.RoyalBlue

    // --- 2. Testing getAnnotationColor (Backgrounds) ---

    [<Fact>]
    let ``getAnnotationColor returns very light backgrounds`` () =
        let mainBg = RepertoireUILogic.getAnnotationColor MainLine
        let altBg = RepertoireUILogic.getAnnotationColor Alternative
        let oppBg = RepertoireUILogic.getAnnotationColor Opponent

        // Verify they are different colors
        mainBg |> should not' (equal altBg)
        
        // Verify specific ARGB if needed (ensures your palette doesn't change by accident)
        mainBg.G |> should be (greaterThan 250uy) // It's a very light green
        oppBg.R |> should equal oppBg.G          // Grays have equal R and G

    // Data provider for the Theory
    let allAnnotations : obj[][] = [| 
        [| MainLine |]
        [| Alternative |]
        [| Opponent |] 
    |]

    [<Theory>]
    [<MemberData(nameof(allAnnotations))>]
    let ``Background colors are distinct from white`` (annot: MoveAnnotation) =
        let bg = RepertoireUILogic.getAnnotationColor annot
        bg |> should not' (equal Color.White)