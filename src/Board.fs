module Board

open Quantum
open Gates
open Circuit


type NodeVisibility =
    | Normal
    | Invisible
    | HideInputs
    | HideOutputs

type Node =
    { Definition: NodeDefinition
      Visibility: NodeVisibility }


type Wire =
    { Placement: WirePlacement
      Visible: bool }


type Board =
    { StartNodeId: NodeId
      EndNodeId: NodeId
      Nodes: Map<NodeId, Node>
      Wires: Map<WireId, Wire> }
