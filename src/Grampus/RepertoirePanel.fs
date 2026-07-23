namespace GrampusUI

open System.Drawing
open System.Windows.Forms
open Grampus

type RepertoirePanel() as this =
    inherit UserControl()

    let tree = new TreeView(
        Dock = DockStyle.Fill,
        FullRowSelect = true,
        HideSelection = false,
        Indent = 15
    )

    let txtComment = new TextBox(
        Multiline = true,
        Dock = DockStyle.Fill,
        ScrollBars = ScrollBars.Vertical,
        Font = new Font("Segoe UI", 10.0f)
    )

    let split = new SplitContainer(
        Orientation = Orientation.Horizontal,
        Dock = DockStyle.Fill,
        SplitterDistance = 300
    )

    // Events
    let moveSelected = new Event<Mv>()
    let commentUpdated = new Event<RepertoireNode * string>()

    do
        // Layout
        let commentHeader = new Label(Text = "Comment:", Dock = DockStyle.Top, Height = 20)
        let bottomPanel = new Panel(Dock = DockStyle.Fill)
        bottomPanel.Controls.Add(txtComment)
        bottomPanel.Controls.Add(commentHeader)

        split.Panel1.Controls.Add(tree)
        split.Panel2.Controls.Add(bottomPanel)
        this.Controls.Add(split)

        // Tree Events
        tree.AfterSelect.Add(fun e ->
            match e.Node.Tag with
            | :? RepertoireNode as node -> 
                txtComment.Text <- node.Comment
            | _ -> txtComment.Text <- ""
        )

        tree.NodeMouseDoubleClick.Add(fun e ->
            match e.Node.Tag with
            | :? RepertoireNode as node -> moveSelected.Trigger(node.Mv)
            | _ -> ()
        )

        // Comment Events: Update when focus is lost or text changes
        txtComment.LostFocus.Add(fun _ ->
            if tree.SelectedNode <> null then
                match tree.SelectedNode.Tag with
                | :? RepertoireNode as node -> 
                    if node.Comment <> txtComment.Text then
                        commentUpdated.Trigger(node, txtComment.Text)
                | _ -> ()
        )

    /// Recursive helper to build tree nodes
    let rec createTreeNode (node: RepertoireNode): TreeNode =
        let tn = new TreeNode(San.ToFigurine node.San)
        tn.Tag <- node
        for reply in node.Replies do
            tn.Nodes.Add(createTreeNode reply) |> ignore
        tn

    [<CLIEvent>] member this.OnMoveSelected = moveSelected.Publish
    [<CLIEvent>] member this.OnCommentUpdated = commentUpdated.Publish
    member this.UpdateFullTree(repertoire: Repertoire) =
        let updateAction() =
            tree.SuspendLayout()
            tree.Nodes.Clear()
            let rootNode = new TreeNode(repertoire.Name)
            for r in repertoire.Roots do
                rootNode.Nodes.Add(createTreeNode r) |> ignore
            tree.Nodes.Add(rootNode) |> ignore
            rootNode.Expand()
            tree.ResumeLayout()

        if this.IsHandleCreated then 
            this.BeginInvoke(MethodInvoker(updateAction)) |> ignore 
        else updateAction()
    member this.Clear() =
        if this.IsHandleCreated then 
            this.BeginInvoke(MethodInvoker(fun () -> tree.Nodes.Clear())) |> ignore
        else tree.Nodes.Clear()