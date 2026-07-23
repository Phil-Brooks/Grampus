namespace GrampusUI

open System
open System.Drawing
open System.Windows.Forms
open Grampus

type RepertoirePanel() as this =
    inherit UserControl()

    let grid = new DataGridView(
        Dock = DockStyle.Fill, AllowUserToAddRows = false, ReadOnly = true,
        RowHeadersVisible = false, BackgroundColor = Color.White,
        SelectionMode = DataGridViewSelectionMode.FullRowSelect,
        EnableHeadersVisualStyles = false,
        AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None
    )

    // Event to notify the main form that a move was selected from the book
    let moveSelected = new Event<Mv>()

    do
        grid.RowTemplate.Height <- 28
        grid.ColumnHeadersDefaultCellStyle.BackColor <- Color.White
        grid.Columns.Add("Move", "Move") |> ignore
        grid.Columns.Add("Comment", "Comment") |> ignore
        grid.Columns.[0].Width <- 60
        grid.Columns.[1].AutoSizeMode <- DataGridViewAutoSizeColumnMode.Fill
        grid.DefaultCellStyle.SelectionBackColor <- Color.White
        grid.DefaultCellStyle.SelectionForeColor <- Color.Black
        grid.ColumnHeadersDefaultCellStyle.SelectionBackColor <- Color.White
        grid.ColumnHeadersDefaultCellStyle.SelectionForeColor <- Color.Black


        // Handle double clicking a move to play it
        grid.CellDoubleClick.Add(fun e ->
            if e.RowIndex >= 0 then
                match grid.Rows.[e.RowIndex].Tag with
                | :? RepertoireNode as node -> moveSelected.Trigger(node.Mv)
                | _ -> ()
        )

        this.Controls.Add(grid)

    [<CLIEvent>] member this.OnMoveSelected = moveSelected.Publish
    member this.UpdateMoves(nodes: RepertoireNode list) =
        let updateAction() =
            grid.SuspendLayout()
            grid.Rows.Clear()
            for node in nodes do
                let san = San.ToFigurine node.San
                let rowIdx = grid.Rows.Add([| box san; box node.Comment |])
                grid.Rows.[rowIdx].Tag <- node
            grid.ResumeLayout()
        // If the window is already visible, use BeginInvoke to be thread-safe
        if this.IsHandleCreated then
            this.BeginInvoke(MethodInvoker(updateAction)) |> ignore
        else
            // If we are still in the constructor (startup), just run it directly
            updateAction()
    member this.Clear() =
        let clearAction() = grid.Rows.Clear()
        if this.IsHandleCreated then
            this.BeginInvoke(MethodInvoker(clearAction)) |> ignore
        else
            clearAction()