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

let inputWireIds circuit nodeId =
    seq {
        for wire in circuit.Wires do
            if wire.Value.Right.NodeId = nodeId then yield wire.Value.Right.Port, wire.Key
    }
    |> Seq.sortBy fst
    |> Seq.map snd
    |> List.ofSeq

let outputWireIds circuit nodeId =
    seq {
        for wire in circuit.Wires do
            if wire.Value.Left.NodeId = nodeId then yield wire.Value.Left.Port, wire.Key
    }
    |> Seq.sortBy fst
    |> Seq.map snd
    |> List.ofSeq


type private EvaluatorState =
    { State: MixedState
      Previous: NodeId Set }

let rec private evalNode circuit nodeId evalState =
    if Set.contains nodeId evalState.Previous then
        evalState
    else
        let inputs = inputWireIds circuit nodeId
        let outputs = outputWireIds circuit nodeId

        let evalState =
            Seq.fold (fun es wireId -> evalNode circuit circuit.Wires.[wireId].Left.NodeId es) evalState inputs

        printfn "%s" ((inputs, outputs).ToString())
        { State = circuit.Nodes.[nodeId] (inputs, outputs) evalState.State
          Previous = Set.add nodeId evalState.Previous }

let eval circuit initialState =
    (circuit.Nodes
     |> Seq.fold (fun s n -> evalNode circuit n.Key s)
            { State = initialState
              Previous = Set.empty }).State
