module SeattleQio.Simulator.Circuit

open SeattleQio.Simulator.Gates
open SeattleQio.Simulator.Quantum

type NodeId = NodeId of int

type NodeOutputId = { NodeId: NodeId; OutputPort: int }

type NodeInputId = { NodeId: NodeId; InputPort: int }

type NodeIOId =
    | NodeInputId of NodeInputId
    | NodeOutputId of NodeOutputId
    
module NodeIOId =
    let nodeId =
        function
        | NodeInputId input -> input.NodeId
        | NodeOutputId output -> output.NodeId

type WirePlacement =
    { Left: NodeOutputId
      Right: NodeInputId }

type Circuit =
    { Nodes: Map<NodeId, Gate>
      Wires: Map<WireId, WirePlacement> }

let inputWireIds circuit nodeId =
    seq {
        for wire in circuit.Wires do
            if wire.Value.Right.NodeId = nodeId then yield wire.Value.Right.InputPort, wire.Key
    }
    |> Seq.sortBy fst
    |> Seq.map snd
    |> List.ofSeq

let outputWireIds circuit nodeId =
    seq {
        for wire in circuit.Wires do
            if wire.Value.Left.NodeId = nodeId then yield wire.Value.Left.OutputPort, wire.Key
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

        // printfn "Evaluated node %s, state is %s" (nodeId.ToString()) (prettyPrint evalState.State)
        { State = circuit.Nodes.[nodeId] (inputs, outputs) evalState.State
          Previous = Set.add nodeId evalState.Previous }

let eval circuit initialState =
    (circuit.Nodes
     |> Seq.fold (fun s n -> evalNode circuit n.Key s)
            { State = initialState
              Previous = Set.empty })
        .State
