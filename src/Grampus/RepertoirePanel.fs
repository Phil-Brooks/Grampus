namespace GrampusUI

open System.Drawing
open System.Windows.Forms
open Grampus

type RepertoirePanel() as this =
    inherit UserControl()

    let mutable isInternalAction = false
    let tree = new TreeView(
        Dock = DockStyle.Fill,
        FullRowSelect = true,
        HideSelection = false,
        Indent = 20,
        ShowLines = false,
        Font = new Font("Segoe UI Symbol", 10.0f) 
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
    let movesSelected = new Event<Mv list>()
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
        let handleToggle (e: TreeViewCancelEventArgs) =
            // 1. If we are expanding via code (UpdateFullTree), allow it.
            if isInternalAction then ()
            // 2. If the user clicked the +/- icon specifically, the action 
            // will be Expand or Collapse. Allow these.
            else
                // 3. Otherwise, it's a double-click (Action = Unknown). 
                // Only allow it if the mouse is actually over the +/- icon.
                let clientPos = tree.PointToClient(Cursor.Position)
                let hitTest = tree.HitTest(clientPos)
                if hitTest.Location <> TreeViewHitTestLocations.PlusMinus then
                    e.Cancel <- true        
        
        tree.BeforeCollapse.Add(handleToggle)
        tree.BeforeExpand.Add(handleToggle)

        tree.AfterSelect.Add(fun e ->
            match e.Node.Tag with
            | :? RepertoireNode as node -> 
                txtComment.Text <- node.Comment
            | _ -> txtComment.Text <- ""
        )
        tree.NodeMouseDoubleClick.Add(fun e ->
            // Recursive helper to walk UP the tree to collect moves
            let rec getPath (tn: TreeNode) acc =
                if tn = null then acc
                else
                    match tn.Tag with
                    | :? RepertoireNode as node -> 
                        getPath tn.Parent (node.Mv :: acc) // Add to front of list
                    | _ -> getPath tn.Parent acc // Skip the "White Repertoire" root text node
            
            let path = getPath e.Node []
            if not path.IsEmpty then movesSelected.Trigger(path)
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

    let rec createTreeNode (node: RepertoireNode) : TreeNode =
        let san = San.ToFigurine node.San
        let tn = new TreeNode(san)
        tn.Tag <- node // Store the node so we can access Mv and Comment later
        for reply in node.Replies do
            tn.Nodes.Add(createTreeNode reply) |> ignore
        tn

    [<CLIEvent>] member this.OnMovesSelected = movesSelected.Publish
    [<CLIEvent>] member this.OnCommentUpdated = commentUpdated.Publish

    member this.UpdateFullTree(repertoire: Repertoire) =
        let updateAction() =
            tree.SuspendLayout()
            // Set flag BEFORE clearing to ensure all subsequent events are caught
            isInternalAction <- true 
            
            tree.Nodes.Clear()
            
            let rootNode = new TreeNode(repertoire.Name)
            rootNode.Tag <- null
            
            for r in repertoire.Roots do
                rootNode.Nodes.Add(createTreeNode r) |> ignore
            
            tree.Nodes.Add(rootNode) |> ignore
            
            // Expand everything programmatically
            tree.ExpandAll()
            
            // Explicitly expand the root node for good measure
            if tree.Nodes.Count > 0 then 
                tree.Nodes.[0].Expand()
                tree.Nodes.[0].EnsureVisible()
            
            isInternalAction <- false
            tree.ResumeLayout()

        if this.IsHandleCreated then
            this.BeginInvoke(MethodInvoker(updateAction)) |> ignore
        else
            updateAction()    
    
    member this.Clear() =
        if this.IsHandleCreated then 
            this.BeginInvoke(MethodInvoker(fun () -> tree.Nodes.Clear())) |> ignore
        else tree.Nodes.Clear()