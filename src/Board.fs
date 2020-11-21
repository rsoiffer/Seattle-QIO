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


type WireVisibility =
    | Normal
    | Invisible

type Wire =
    { Placement: WirePlacement
      Visible: WireVisibility }


type Board =
    { Nodes: Map<NodeId, Node>
      Wires: Map<WireId, Wire> }
