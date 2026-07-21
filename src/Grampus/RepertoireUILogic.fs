namespace Grampus

open System.Drawing

module RepertoireUILogic =
    let getAnnotationDetails (annot: MoveAnnotation) =
        match annot with
        | MoveAnnotation.MainLine -> "Main", Color.DarkGreen, FontStyle.Bold
        | MoveAnnotation.Alternative -> "Alt", Color.RoyalBlue, FontStyle.Regular
        | MoveAnnotation.Opponent -> "Opp", Color.DimGray, FontStyle.Italic

    let getAnnotationColor (annot: MoveAnnotation) =
        match annot with
        | MoveAnnotation.MainLine -> Color.FromArgb(235, 255, 235)    // Very light green
        | MoveAnnotation.Alternative -> Color.FromArgb(235, 245, 255) // Very light blue
        | MoveAnnotation.Opponent -> Color.FromArgb(245, 245, 245)    // Light gray