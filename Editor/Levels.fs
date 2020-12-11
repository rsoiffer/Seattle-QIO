module internal SeattleQio.Editor.Levels

open SeattleQio.Editor.Board
open SeattleQio.Simulator
open SeattleQio.Simulator.Circuit
open SeattleQio.Simulator.Gates
open SeattleQio.Simulator.Quantum

type Challenge =
    { Description: string
      Free: NodeDefinition list
      Costly: (NodeDefinition * int) list
      Goal: NodeDefinition }

type Level = { Challenge: Challenge; Board: Board }

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
          InferredInputTypes = None
          InferredOutputTypes = None
          Position = { X = 0.0; Y = 0.0 } }

    let endNode =
        { Definition = endNodeDef challenge.Goal.Outputs
          InferredInputTypes = None
          InferredOutputTypes = None
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

let testOnce level =
    let circuit = toCircuit level.Board

    let startWireIds =
        outputWireIds circuit level.Board.StartNodeId

    let endWireIds =
        inputWireIds circuit level.Board.EndNodeId

    let classicalStartWires =
        seq {
            for i in idx level.Challenge.Goal.Inputs do
                if level.Challenge.Goal.Inputs.[i].DataType = Classical
                then yield startWireIds.[i]
        }

    let quantumStartWires =
        seq {
            yield WireId 0

            for i in idx level.Challenge.Goal.Inputs do
                if level.Challenge.Goal.Inputs.[i].DataType = Quantum
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
                          NodeId 1, level.Challenge.Goal.Gate
                          NodeId 2, gate_DoNothing ]
          Wires =
              Map.ofSeq
                  (Seq.concat [ idx level.Challenge.Goal.Inputs
                                |> Seq.map (fun i ->
                                    startWireIds.[i],
                                    { Left = { NodeId = NodeId 0; OutputPort = i }
                                      Right = { NodeId = NodeId 1; InputPort = i } })
                                idx level.Challenge.Goal.Outputs
                                |> Seq.map (fun i ->
                                    endWireIds.[i],
                                    { Left = { NodeId = NodeId 1; OutputPort = i }
                                      Right = { NodeId = NodeId 2; InputPort = i } }) ]) }

    let realOutputState = eval circuit inputState
    let oracleOutputState = eval oracleCircuit inputState

    // printfn "%A" (norm quantumState)
    // printfn "%s" (prettyPrint inputState)
    // printfn "%s" (prettyPrint realOutputState)
    // printfn "%s" (prettyPrint oracleOutputState)

    realOutputState, oracleOutputState


let emptyBoardFrom nodeDef =
    { StartNodeId = NodeId 0
      EndNodeId = NodeId 1
      Nodes =
          [ NodeId 0,
            { Definition = startNodeDef nodeDef.Inputs
              InferredInputTypes = None
              InferredOutputTypes = None
              Position = { X = 50.0; Y = 100.0 } }
            NodeId 1,
            { Definition = endNodeDef nodeDef.Outputs
              InferredInputTypes = None
              InferredOutputTypes = None
              Position = { X = 800.0; Y = 100.0 } } ]
          |> Map.ofList
      Wires = Map.empty
      WireCreationState = NotDragging }

let level_quantumCoinFlip =
    { Challenge =
        { Description = "A quantum coin flip: measure zero or one 50% of the time."
          Free = [ InitQubit; X; Z; H; M ]
          Costly = [ ]
          Goal = InitCbitRandom }
      Board = emptyBoardFrom InitCbitRandom }

let level_swap =
    { Challenge =
        { Description = "Swap Alice and Bob's qubits"
          Free = [ ]
          Costly = [ qbit_AB, 1; qbit_BA, 1 ]
          Goal = SWAP }
      Board = emptyBoardFrom SWAP }

let level_swap_cnot =
    { Challenge =
        { Description = "Swap Alice and Bob's qubits"
          Free = [ ]
          Costly = [ CNOT_AB, 2; CNOT_BA, 1 ]
          Goal = SWAP }
      Board = emptyBoardFrom SWAP }

let level_qbit_to_ebit =
    { Challenge =
        { Description = "Create an entangled pair"
          Free = [ InitQubit; H; CNOT ]
          Costly = [ qbit_AB, 1 ]
          Goal = ebit }
      Board = emptyBoardFrom ebit }

let level_qbit_to_cbit =
    { Challenge =
        { Description = "Send a classical bit from Alice to Bob"
          Free = [ InitQubit; M; Controlled_X; DestroyCbit ]
          Costly = [ qbit_AB, 1 ]
          Goal = cbit_AB }
      Board = emptyBoardFrom cbit_AB }


