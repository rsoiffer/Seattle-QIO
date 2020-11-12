module BoardState

open Quantum
open GateImplementations


type NodeId = NodeId of int
type NodeOutputId = { NodeId: NodeId; Port: int }
type NodeInputId = { NodeId: NodeId; Port: int }

type WirePlacement =
    { Left: NodeOutputId
      Right: NodeInputId option }

type BoardState =
    { Nodes: Map<NodeId, GateImplementation>
      Wires: Map<WireId, WirePlacement> }


type EvaluationState =
    { State: SystemState
      Previous: NodeId Set }

let rec evalNode boardState nodeId evalState =
    if Set.contains nodeId evalState.Previous then
        evalState
    else
        let mutable evalState = evalState
        for wire in boardState.Wires do
            match wire.Value.Right with
            | Some inputPlacement ->
                if inputPlacement.NodeId = nodeId
                then evalState <- evalNode boardState wire.Value.Left.NodeId evalState
            | _ -> ()

        let outputs =
            seq {
                for wire in boardState.Wires do
                    if wire.Value.Left.NodeId = nodeId then yield wire.Value.Left.Port, wire.Key
            }
            |> Seq.sortBy fst
            |> Seq.map snd
            |> List.ofSeq

        let inputs =
            seq {
                for wire in boardState.Wires do
                    match wire.Value.Right with
                    | Some inputPlacement -> if inputPlacement.NodeId = nodeId then yield inputPlacement.Port, wire.Key
                    | None -> ()
            }
            |> Seq.sortBy fst
            |> Seq.map snd
            |> List.ofSeq

        printfn "%s" ((inputs, outputs).ToString())
        { State = boardState.Nodes.[nodeId] (inputs, outputs) evalState.State
          Previous = Set.add nodeId evalState.Previous }

let eval boardState =
    let mutable evalState =
        { State = emptyState
          Previous = Set.empty }

    for node in boardState.Nodes do
        evalState <- evalNode boardState node.Key evalState
    evalState
