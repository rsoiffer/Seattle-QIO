module Board

open Quantum
open Circuit
open NodeDefinitions

type NodeVisibility =
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
