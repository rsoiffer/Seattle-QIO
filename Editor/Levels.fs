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
      Goals: NodeDefinition list }

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

let toCircuit (board: Board) =
    { Nodes = Map.map (fun _ node -> (info node.Definition).Gate) board.Nodes
      Wires = Map.map (fun _ wire -> wire.Placement) board.Wires }

let idx s = seq { 0 .. Seq.length s - 1 }

let randomInput level =
    let circuit = toCircuit level.Board

    let startWireIds =
        [ for startNodeId in level.Board.StartNodeIds do
            yield outputWireIds circuit startNodeId ]

    let classicalStartWires =
        [ for j in idx level.Challenge.Goals do
            for i in idx (info level.Challenge.Goals.[j]).Inputs do
                if (info level.Challenge.Goals.[j]).Inputs.[i].DataType = Classical
                then yield startWireIds.[j].[i] ]

    let quantumStartWires =
        [ for j in idx level.Challenge.Goals do
            for i in idx (info level.Challenge.Goals.[j]).Inputs do
                if (info level.Challenge.Goals.[j]).Inputs.[i].DataType = Quantum
                then yield startWireIds.[j].[i] ]

    let classicalState = randomClassicalState classicalStartWires

    let quantumState = randomPureState quantumStartWires

    let fullState =
        SparseVector.tensor (SparseVector.ofSeq [ classicalState, Complex.one ]) quantumState
        |> SparseVector.map (fun (b1, b2) -> merge b1 b2)

    outer fullState fullState

let testOnce level =
    let circuit = toCircuit level.Board

    let startWireIds =
        [ for startNodeId in level.Board.StartNodeIds do
            yield outputWireIds circuit startNodeId ]

    let endWireIds =
        [ for endNodeId in level.Board.EndNodeIds do
            yield inputWireIds circuit endNodeId ]

    let inputState = randomInput level

    let oracleCircuit =
        { Nodes =
              Map.ofSeq [ yield NodeId -1, gate_DoNothing
                          for j in idx level.Challenge.Goals do
                              yield NodeId j, (info level.Challenge.Goals.[j]).Gate
                          yield NodeId -2, gate_DoNothing ]
          Wires =
              Map.ofSeq [ for j in idx level.Challenge.Goals do
                              for i in idx (info level.Challenge.Goals.[j]).Inputs do
                                  yield
                                      startWireIds.[j].[i],
                                      { Left = { NodeId = NodeId -1; OutputPort = i }
                                        Right = { NodeId = NodeId j; InputPort = i } }
                              for i in idx (info level.Challenge.Goals.[j]).Outputs do
                                  yield
                                      endWireIds.[j].[i],
                                      { Left = { NodeId = NodeId j; OutputPort = i }
                                        Right = { NodeId = NodeId -2; InputPort = i } } ] }

    let realOutputState = eval circuit inputState
    let oracleOutputState = eval oracleCircuit inputState

    printfn "%s" (prettyPrint inputState)
    printfn "%s" (prettyPrint realOutputState)
    printfn "%s" (prettyPrint oracleOutputState)

    inputState, realOutputState, oracleOutputState


let emptyLevelFrom challenge =
    { Challenge = challenge
      Board =
          { StartNodeIds =
                [ for i in idx challenge.Goals do
                    if List.length (info challenge.Goals.[i]).Inputs > 0
                    then yield NodeId i ]
            EndNodeIds =
                [ for i in idx challenge.Goals do
                    yield NodeId(100 + i) ]
            Nodes =
                [ for i in idx challenge.Goals do
                    if List.length (info challenge.Goals.[i]).Inputs > 0 then
                        yield
                            NodeId i,
                            { Definition = StartNode ((info challenge.Goals.[i]).Inputs, (i + 1))
                              InferredInputTypes = None
                              InferredOutputTypes = None
                              Position = { X = 50.0; Y = 75.0 * (float i + 1.0) } }

                    yield
                        NodeId(100 + i),
                        { Definition = EndNode ((info challenge.Goals.[i]).Outputs, (i + 1))
                          InferredInputTypes = None
                          InferredOutputTypes = None
                          Position =
                              { X = 750.0
                                Y = 75.0 * (float i + 1.0) } } ]
                |> Map.ofList
            Wires = Map.empty
            WireCreationState = NotDragging } }

let challenge_quantumCoinFlip =
    { Description = "A quantum coin flip: measure zero or one 50% of the time."
      Free = [ InitQubit; X; Z; H; M ]
      Costly = []
      Goals = [ InitCbitRandom ] }

let challenge_swap =
    { Description = "Swap Alice and Bob's qubits"
      Free = []
      Costly = [ Qbit_AB, 1; Qbit_BA, 1 ]
      Goals = [ SWAP ] }

let challenge_swap_cnot =
    { Description = "Swap Alice and Bob's qubits"
      Free = []
      Costly = [ CNOT_AB, 2; CNOT_BA, 1 ]
      Goals = [ SWAP ] }

let challenge_qbit_to_ebit =
    { Description = "Create an entangled pair"
      Free = [ InitQubit; H; CNOT ]
      Costly = [ Qbit_AB, 1 ]
      Goals = [ Ebit ] }

let challenge_qbit_to_cbit =
    { Description = "Send a classical bit from Alice to Bob"
      Free =
          [ InitQubit
            M
            Controlled_X
            DestroyCbit ]
      Costly = [ Qbit_AB, 1 ]
      Goals = [ Cbit_AB ] }


let challenge_a1q1_a =
    { Description = "Implement the CNOT gate"
      Free =
          [ InitQubit
            H
            CNOT
            M
            Controlled_X
            Controlled_Z
            DestroyCbit ]
      Costly = [ Ebit, 1; Cbit_AB, 1; Cbit_BA, 1 ]
      Goals = [ CNOT_AB ] }

let challenge_a1q1_b =
    { Description = "Implement the CNOT gate and generate two ebits"
      Free = [ X; Z; H; CNOT; CZ ]
      Costly = [ Ebit, 1; Cobit_AB, 1; Cobit_BA, 1 ]
      Goals = [ CNOT_AB; Ebit; Ebit ] }

let challenge_a1q1_c =
    { Description = "Send two cbits, one in each direction"
      Free =
          [ InitQubit
            H
            CNOT
            M
            Controlled_X
            Controlled_Z
            DestroyCbit ]
      Costly = [ CNOT_AB, 1; Ebit, 1 ]
      Goals = [ Cbit_AB; Cbit_BA ] }

let challenge_a1q1_d =
    { Description = "Send two cobits, one in each direction"
      Free = [ X; Z; H; CNOT; CZ ]
      Costly = [ CNOT_AB, 1; Ebit, 1 ]
      Goals = [ Cobit_AB; Cobit_BA ] }

let challenge_a1q1_e =
    { Description = "SWAP two qubits and generate 3 ebits"
      Free = [ X; Z; H; CNOT; CZ ]
      Costly = [ CNOT_AB, 1; CNOT_BA, 1; Ebit, 3 ]
      Goals = [ SWAP; Ebit; Ebit; Ebit ] }
