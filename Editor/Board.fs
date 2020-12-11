namespace SeattleQio.Editor.Board

open SeattleQio.Editor.Collections
open SeattleQio.Editor.React.Draggable
open SeattleQio.Simulator.Circuit
open SeattleQio.Simulator.Gates
open SeattleQio.Simulator.Quantum

type internal Position = { X: float; Y: float }

module internal Position =
    let toDraggable { X = x; Y = y } = { x = x; y = y }

type internal NodeVisibility =
    | Normal
    | Invisible
    | HideInputs
    | HideOutputs

type internal Node =
    { Definition: NodeDefinition
      Visibility: NodeVisibility
      Position: Position }

type internal Wire =
    { Placement: WirePlacement
      Visible: bool }

type internal WireCreationState =
    | NotDragging
    | FloatingRight of NodeOutputId * Position
    | FloatingLeft of NodeInputId * Position

type internal Board =
    { StartNodeId: NodeId
      EndNodeId: NodeId
      Nodes: Map<NodeId, Node>
      Wires: Map<WireId, Wire>
      WireCreationState: WireCreationState }

module internal Board =
    let myRandom = System.Random()

    let addNode node (board: Board) =
        let nodeId = NodeId(myRandom.Next())

        { board with
              Nodes = board.Nodes |> Map.add nodeId node }

    let removeNode nodeId board: Board =
        let wires =
            board.Wires
            |> Map.filter (fun _ wire ->
                wire.Placement.Left.NodeId
                <> nodeId
                && wire.Placement.Right.NodeId <> nodeId)

        { board with
              Nodes = board.Nodes |> Map.remove nodeId
              Wires = wires }

    let randomizeNodeIds board: Board =
        let newNodeIds =
            board.Nodes
            |> Map.map (fun k v -> NodeId(myRandom.Next()))

        let newNodes =
            board.Nodes
            |> Map.toSeq
            |> Seq.map (fun (a, b) -> (newNodeIds.[a], b))
            |> Map.ofSeq

        let newWires =
            board.Wires
            |> Map.map (fun k wire ->
                let newLeft =
                    { wire.Placement.Left with
                          NodeId = newNodeIds.[wire.Placement.Left.NodeId] }

                let newRight =
                    { wire.Placement.Right with
                          NodeId = newNodeIds.[wire.Placement.Right.NodeId] }

                { wire with
                      Placement = { Left = newLeft; Right = newRight } })

        { board with
              Nodes = newNodes
              Wires = newWires }

    let addWire (left: NodeOutputId) (right: NodeInputId) (board: Board) =
        let wireId = WireId(myRandom.Next())

        let wire =
            { Placement = { Left = left; Right = right }
              Visible = true }

        { board with
              Wires =
                  board.Wires
                  |> Map.filter (fun _ wire ->
                      wire.Placement.Left
                      <> left
                      && wire.Placement.Right <> right)
                  |> Map.add wireId wire }

    let port nodeIoId board =
        match nodeIoId with
        | NodeInputId nodeInputId -> board.Nodes.[nodeInputId.NodeId].Definition.Inputs.[nodeInputId.InputPort]
        | NodeOutputId nodeInputId -> board.Nodes.[nodeInputId.NodeId].Definition.Outputs.[nodeInputId.OutputPort]

    let count definition board =
        board.Nodes
        |> Map.filter (fun _ node -> node.Definition = definition)
        |> Map.count
