module Circuit

open Quantum
open Gates


type NodeId = NodeId of int
type NodeOutputId = { NodeId: NodeId; Port: int }
type NodeInputId = { NodeId: NodeId; Port: int }

type WirePlacement =
    { Left: NodeOutputId
      Right: NodeInputId }

type Circuit =
    { Nodes: Map<NodeId, Gate>
      Wires: Map<WireId, WirePlacement> }

type EvaluatorState =
    { State: MixedState
      Previous: NodeId Set }

let rec evalNode circuit nodeId evalState =
    if Set.contains nodeId evalState.Previous then
        evalState
    else
        let mutable evalState = evalState
        for wire in circuit.Wires do
            if wire.Value.Right.NodeId = nodeId
            then evalState <- evalNode circuit wire.Value.Left.NodeId evalState

        let outputs =
            seq {
                for wire in circuit.Wires do
                    if wire.Value.Left.NodeId = nodeId then yield wire.Value.Left.Port, wire.Key
            }
            |> Seq.sortBy fst
            |> Seq.map snd
            |> List.ofSeq

        let inputs =
            seq {
                for wire in circuit.Wires do
                    if wire.Value.Right.NodeId = nodeId then yield wire.Value.Right.Port, wire.Key
            }
            |> Seq.sortBy fst
            |> Seq.map snd
            |> List.ofSeq

        printfn "%s" ((inputs, outputs).ToString())
        { State = circuit.Nodes.[nodeId] (inputs, outputs) evalState.State
          Previous = Set.add nodeId evalState.Previous }

let eval circuit =
    circuit.Nodes
    |> Seq.fold (fun s n -> evalNode circuit n.Key s) { State = Rho []; Previous = Set.empty }
