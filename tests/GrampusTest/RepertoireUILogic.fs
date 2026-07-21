namespace Grampus.Tests

open Xunit
open FsUnit.Xunit
open Grampus
open System.Drawing

module RepertoireUILogic =

    // --- 1. Testing getAnnotationDetails ---
    
    // Define the test data as a static member
    let annotationCases : obj[][] = [|
        [| MoveAnnotation.MainLine; "Main"; FontStyle.Bold |]
        [| MoveAnnotation.Alternative; "Alt"; FontStyle.Regular |]
        [| MoveAnnotation.Opponent; "Opp"; FontStyle.Italic |]
    |]

    [<Theory>]
    [<MemberData(nameof(annotationCases))>]
    let ``getAnnotationDetails returns correct text and style`` (annot: MoveAnnotation) (expectedText: string) (expectedStyle: FontStyle) =
        let text, _, style = RepertoireUILogic.getAnnotationDetails annot
        
        text |> should equal expectedText
        style |> should equal expectedStyle
    
    [<Fact>]
    let ``getAnnotationDetails returns specific colors for specific types`` () =
        let _, mainCol, _ = RepertoireUILogic.getAnnotationDetails MoveAnnotation.MainLine
        let _, altCol, _ = RepertoireUILogic.getAnnotationDetails MoveAnnotation.Alternative
        
        mainCol |> should equal Color.DarkGreen
        altCol |> should equal Color.RoyalBlue

    // --- 2. Testing getAnnotationColor (Backgrounds) ---

    [<Fact>]
    let ``getAnnotationColor returns very light backgrounds`` () =
        let mainBg = RepertoireUILogic.getAnnotationColor MoveAnnotation.MainLine
        let altBg = RepertoireUILogic.getAnnotationColor MoveAnnotation.Alternative
        let oppBg = RepertoireUILogic.getAnnotationColor MoveAnnotation.Opponent

        // Verify they are different colors
        mainBg |> should not' (equal altBg)
        
        // Verify specific ARGB if needed (ensures your palette doesn't change by accident)
        mainBg.G |> should be (greaterThan 250uy) // It's a very light green
        oppBg.R |> should equal oppBg.G          // Grays have equal R and G

    // Data provider for the Theory
    let allAnnotations : obj[][] = [| 
        [| MoveAnnotation.MainLine |]
        [| MoveAnnotation.Alternative |]
        [| MoveAnnotation.Opponent |] 
    |]

    [<Theory>]
    [<MemberData(nameof(allAnnotations))>]
    let ``Background colors are distinct from white`` (annot: MoveAnnotation) =
        let bg = RepertoireUILogic.getAnnotationColor annot
        bg |> should not' (equal Color.White)