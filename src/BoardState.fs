module BoardState

open Quantum

type WireDataType =
    | Classical
    | Quantum

type Party =
    | Alice
    | Bob

type WireType =
    { dataType: WireDataType
      party: Party }

type NodeType =
    { inputs: WireType list
      outputs: WireType list }

type NodeID = int
type NodeOutputID = NodeID * int
type NodeInputID = NodeID * int

type Node =
    { name: string
      position: float * float
      nodeType: NodeType }

type WirePlacement =
    { output: NodeOutputID
      input: NodeInputID }

type Wire =
    { wireType: WireType
      placement: WirePlacement }

type BoardState =
    { nodes: Map<NodeID, Node>
      wires: Map<WireID, Wire> }
