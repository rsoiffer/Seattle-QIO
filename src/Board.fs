module Board

open Quantum
open Circuit
open NodeDefinitions


type NodeVisibility =
    | Normal
    | Invisible
    | HideInputs
    | HideOutputs

type Node =
    { Definition: NodeDefinition
      Visibility: NodeVisibility }


type WireVisibility =
    | Normal
    | Invisible

type Wire =
    { Placement: WirePlacement
      Visible: WireVisibility }


type Board =
    { Nodes: Map<NodeId, Node>
      Wires: Map<WireId, Wire> }
