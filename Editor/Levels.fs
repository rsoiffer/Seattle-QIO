module internal SeattleQio.Editor.Levels

open SeattleQio.Editor.Board
open SeattleQio.Simulator
open SeattleQio.Simulator.Circuit
open SeattleQio.Simulator.Gates
open SeattleQio.Simulator.Quantum

type Challenge =
    { Free: NodeDefinition list
      Costly: (NodeDefinition * int) list
      Goal: NodeDefinition }

let private myRandom = System.Random()

let randomClassicalState wireIds =
    let allBits = allPossibleBits wireIds |> Array.ofSeq
    allBits.[myRandom.Next(allBits.Length)]

let randomPureState wireIds =
    let allBits = allPossibleBits wireIds |> List.ofSeq

    let r =
        allBits
        |> List.map (fun _ -> Complex(myRandom.NextDouble(), myRandom.NextDouble()))

    let norm =
        r |> List.sumBy (fun a -> a.Magnitude ** 2.0)

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
          Visibility = Normal
          Position = { X = 0.0; Y = 0.0 } }

    let endNode =
        { Definition = endNodeDef challenge.Goal.Outputs
          Visibility = Normal
          Position = { X = 200.0; Y = 0.0 } }

    { StartNodeId = startId
      EndNodeId = endId
      Nodes =
          Map.ofSeq [ startId, startNode
                      endId, endNode ]
      Wires = Map.ofSeq []
      WireCreationState = NotDragging }

let toCircuit (board: Board) =
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

    // printfn "%A" (norm quantumState)
    // printfn "%s" (prettyPrint inputState)
    // printfn "%s" (prettyPrint realOutputState)
    // printfn "%s" (prettyPrint oracleOutputState)

    realOutputState, oracleOutputState
