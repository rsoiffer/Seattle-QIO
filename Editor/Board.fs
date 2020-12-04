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
    let addNode node board =
        let nodeId =
            Map.toSeq board.Nodes
            |> Seq.map (fun ((NodeId nodeId), _) -> nodeId)
            |> Seq.tryMax
            |> Option.defaultValue 0
            |> (+) 1
            |> NodeId

        { board with
              Nodes = board.Nodes |> Map.add nodeId node }

    let removeNode nodeId board: Board =
        let wires =
            board.Wires
            |> Map.filter (fun _ wire ->
                wire.Placement.Left.NodeId <> nodeId
                && wire.Placement.Right.NodeId <> nodeId)

        { board with
              Nodes = board.Nodes |> Map.remove nodeId
              Wires = wires }

    let addWire (left: NodeOutputId) (right: NodeInputId) board =
        let wireId =
            Map.toSeq board.Wires
            |> Seq.map (fun ((WireId wireId), _) -> wireId)
            |> Seq.tryMax
            |> Option.defaultValue 0
            |> (+) 1
            |> WireId

        let wire =
            { Placement = { Left = left; Right = right }
              Visible = true }

        { board with
              Wires =
                  board.Wires
                  |> Map.filter (fun _ wire ->
                      wire.Placement.Left <> left
                      && wire.Placement.Right <> right)
                  |> Map.add wireId wire }

    let port nodeId portId isInput board =
        let nodeDef = board.Nodes.[nodeId].Definition
        if isInput then nodeDef.Inputs.[portId] else nodeDef.Outputs.[portId]

    let count definition board =
        board.Nodes
        |> Map.filter (fun _ node -> node.Definition = definition)
        |> Map.count
