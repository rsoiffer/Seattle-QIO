module BoardState

open Quantum
open GateImplementations

type WireDataType =
    | Classical
    | Quantum

type Party =
    | Alice
    | Bob

type WireType =
    { DataType: WireDataType
      Party: Party }

type NodeType =
    { Inputs: WireType list
      Outputs: WireType list }

type NodeId = NodeId of int
type NodeOutputId = { NodeId: NodeId; Port: int }
type NodeInputId = { NodeId: NodeId; Port: int }

type Node =
    { Name: string
      Position: float * float
      NodeType: NodeType
      Implementation: GateImplementation }

type WirePlacement =
    { Output: NodeOutputId
      Input: NodeInputId }

type Wire =
    { WireType: WireType
      Placement: WirePlacement }

type BoardState =
    { Nodes: Map<NodeId, Node>
      Wires: Map<WireId, Wire> }


module Evaluator =

    type EvaluationState =
        { State: SystemState
          Previous: NodeId Set }

    let rec evalNode boardState nodeId evalState =
        if Set.contains nodeId evalState.Previous then
            evalState
        else
            let mutable evalState = evalState
            for wire in boardState.Wires do
                if wire.Value.Placement.Input.NodeId = nodeId
                then evalState <- evalNode boardState wire.Value.Placement.Output.NodeId evalState

            let outputs =
                seq {
                    for wire in boardState.Wires do
                        if wire.Value.Placement.Output.NodeId = nodeId
                        then yield wire.Value.Placement.Output.Port, wire.Key
                }
                |> Seq.sortBy fst
                |> Seq.map snd
                |> List.ofSeq

            let inputs =
                seq {
                    for wire in boardState.Wires do
                        if wire.Value.Placement.Input.NodeId = nodeId
                        then yield wire.Value.Placement.Input.Port, wire.Key
                }
                |> Seq.sortBy fst
                |> Seq.map snd
                |> List.ofSeq

            printfn "%s" ((inputs, outputs).ToString())
            { State = boardState.Nodes.[nodeId].Implementation (inputs, outputs) evalState.State
              Previous = Set.add nodeId evalState.Previous }

    let eval boardState =
        let mutable evalState =
            { State = emptyState
              Previous = Set.empty }

        for node in boardState.Nodes do
            evalState <- evalNode boardState node.Key evalState
        evalState
