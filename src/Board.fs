module Board

open Quantum
open Gates
open Circuit

type Position = { X: float; Y: float }

type NodeVisibility =
    | Normal
    | Invisible
    | HideInputs
    | HideOutputs

type Node =
    { Definition: NodeDefinition
      Visibility: NodeVisibility
      Position: Position }


type Wire =
    { Placement: WirePlacement
      Visible: bool }

type WireCreationState =
    | NotDragging
    | FloatingRight of NodeOutputId
    | FloatingLeft of NodeInputId

type Board =
    { StartNodeId: NodeId
      EndNodeId: NodeId
      Nodes: Map<NodeId, Node>
      Wires: Map<WireId, Wire>
      WireCreationState: WireCreationState }
