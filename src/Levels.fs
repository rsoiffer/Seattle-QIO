module Levels

open ComplexNumbers
open SparseVector
open Quantum
open Gates
open Circuit
open Board

type Challenge =
    { Free: NodeDefinition list
      Costly: (NodeDefinition * int) list
      Goal: NodeDefinition }

let private myRandom = System.Random()

let rec allPossibleBits =
    function
    | [] -> seq { B [] }
    | head :: tail ->
        seq {
            for Bits others in allPossibleBits tail do
                yield Map.add head false others |> Bits
                yield Map.add head true others |> Bits
        }

let randomClassicalState wireIds =
    let allBits = allPossibleBits wireIds |> Array.ofSeq
    allBits.[myRandom.Next(allBits.Length)]

let randomPureState wireIds =
    let allBits = allPossibleBits wireIds |> List.ofSeq

    let r =
        allBits
        |> List.map (fun _ -> Complex(myRandom.NextDouble(), myRandom.NextDouble()))

    let norm = r |> List.sumBy (fun a -> a.Magnitude)
    r
    |> List.map (fun a -> a / Complex(sqrt norm, 0.0))
    |> List.zip allBits
    |> SparseVector.ofSeq


let startNodeDef inputs =
    { Name = "Input"
      Inputs = []
      Outputs = inputs
      Gate = gate_DoNothing }

let endNodeDef outputs =
    { Name = "Output"
      Inputs = outputs
      Outputs = []
      Gate = gate_DoNothing }

let initialBoard challenge =
    let startId = NodeId(myRandom.Next())
    let endId = NodeId(myRandom.Next())

    let startNode =
        { Definition = startNodeDef challenge.Goal.Inputs
          Visibility = Normal }

    let endNode =
        { Definition = endNodeDef challenge.Goal.Outputs
          Visibility = Normal }

    { StartNodeId = startId
      EndNodeId = endId
      Nodes =
          Map.ofSeq [ startId, startNode
                      endId, endNode ]
      Wires = Map.ofSeq [] }

let toCircuit board =
    { Nodes = Map.map (fun _ node -> node.Definition.Gate) board.Nodes
      Wires = Map.map (fun _ wire -> wire.Placement) board.Wires }

let idx s = seq { 0 .. Seq.length s - 1 }

let testOnce challenge board =
    let circuit = toCircuit board
    let startWireIds = outputWireIds circuit board.StartNodeId
    let endWireIds = inputWireIds circuit board.EndNodeId

    let classicalStartWires =
        seq {
            for i in idx challenge.Goal.Inputs do
                if challenge.Goal.Inputs.[i].DataType = Classical
                then yield startWireIds.[i]
        }

    let quantumStartWires =
        seq {
            yield WireId 0
            for i in idx challenge.Goal.Inputs do
                if challenge.Goal.Inputs.[i].DataType = Quantum
                then yield startWireIds.[i]
        }

    let classicalState =
        randomClassicalState (List.ofSeq classicalStartWires)

    let quantumState =
        randomPureState (List.ofSeq quantumStartWires)

    let fullState =
        SparseVector.tensor (SparseVector.ofSeq [ classicalState, Complex.one ]) quantumState
        |> SparseVector.map (fun (b1, b2) -> merge b1 b2)

    let inputState = outer fullState fullState

    let oracleCircuit =
        { Nodes =
              Map.ofSeq [ NodeId 0, gate_DoNothing
                          NodeId 1, challenge.Goal.Gate
                          NodeId 2, gate_DoNothing ]
          Wires =
              Map.ofSeq
                  (Seq.concat [ idx challenge.Goal.Inputs
                                |> Seq.map (fun i ->
                                    startWireIds.[i],
                                    { Left = { NodeId = NodeId 0; Port = i }
                                      Right = { NodeId = NodeId 1; Port = i } })
                                idx challenge.Goal.Outputs
                                |> Seq.map (fun i ->
                                    endWireIds.[i],
                                    { Left = { NodeId = NodeId 1; Port = i }
                                      Right = { NodeId = NodeId 2; Port = i } }) ]) }

    let realOutputState = eval circuit inputState
    let oracleOutputState = eval oracleCircuit inputState

    realOutputState, oracleOutputState
