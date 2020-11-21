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
    { Nodes: Map<NodeId, Node>
      Wires: Map<WireId, Wire> }
