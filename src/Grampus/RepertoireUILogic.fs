namespace Grampus

open System.Drawing

module RepertoireUILogic =
    let getAnnotationDetails (annot: MoveAnnotation) =
        match annot with
        | MainLine -> "Main", Color.DarkGreen, FontStyle.Bold
        | Alternative -> "Alt", Color.RoyalBlue, FontStyle.Regular
        | Opponent -> "Opp", Color.DimGray, FontStyle.Italic

    let getAnnotationColor (annot: MoveAnnotation) =
        match annot with
        | MainLine -> Color.FromArgb(235, 255, 235)    // Very light green
        | Alternative -> Color.FromArgb(235, 245, 255) // Very light blue
        | Opponent -> Color.FromArgb(245, 245, 245)    // Light gray