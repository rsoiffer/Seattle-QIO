module Gates

#nowarn "25" "49"

open ComplexNumbers
open SparseVector
open Quantum

module GateHelpers =

    let bind (func: Bits -> PureState) =
        SparseVector.bind (fun (b1, b2) -> outer (func b1) (func b2))

    let map (func: Bits -> Bits) =
        SparseVector.map (fun (b1, b2) -> func b1, func b2)

    let modifyBits ins f bits =
        let result = List.map (read bits) ins |> f
        bits |> removeAll ins |> merge result

    let modifyBit in' f =
        modifyBits [ in' ] (fun [ val' ] -> f val')

    let copyBits in' outs =
        modifyBit in' (fun val' -> outs |> Seq.map (fun out' -> out', val') |> B)

    let unitary ins f =
        bind (fun bits ->
            List.map (read bits) ins
            |> f
            |> SparseVector.map (bits |> removeAll ins |> merge))

    let unitary1 in' f =
        unitary [ in' ] (fun [ val' ] -> f val')

    let myRandom = System.Random()

open GateHelpers



type Gate = WireId list * WireId list -> MixedState -> MixedState

type DataType =
    | Classical
    | Quantum

type Party =
    | Any
    | Alice
    | Bob

type Port = { DataType: DataType; Party: Party }

let port d p = { DataType = d; Party = p }

type NodeDefinition =
    { Name: string
      Inputs: Port list
      Outputs: Port list
      Gate: Gate }


let gate_DoNothing: Gate =
    function
    | _ -> id


let gate_move: Gate =
    function
    | [ in1 ], [ out1 ] -> map (copyBits in1 [ out1 ])
    | _ -> failwith "wires not correct"

let cbit =
    { Name = "cbit"
      Inputs = [ port Classical Any ]
      Outputs = [ port Classical Any ]
      Gate = gate_move }

let cbit_AB =
    { Name = "cbit_AB"
      Inputs = [ port Classical Alice ]
      Outputs = [ port Classical Bob ]
      Gate = gate_move }

let cbit_BA =
    { Name = "cbit_BA"
      Inputs = [ port Classical Bob ]
      Outputs = [ port Classical Alice ]
      Gate = gate_move }

let qbit =
    { Name = "qbit"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Gate = gate_move }

let qbit_AB =
    { Name = "qbit_AB"
      Inputs = [ port Quantum Alice ]
      Outputs = [ port Quantum Bob ]
      Gate = gate_move }

let qbit_BA =
    { Name = "qbit_BA"
      Inputs = [ port Quantum Bob ]
      Outputs = [ port Quantum Alice ]
      Gate = gate_move }


let gate_copy: Gate =
    function
    | [ in1 ], [ out1; out2 ] -> map (copyBits in1 [ out1; out2 ])
    | _ -> failwith "wires not correct"

let cobit =
    { Name = "cobit"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any; port Quantum Any ]
      Gate = gate_copy }

let cobit_AB =
    { Name = "cobit_AB"
      Inputs = [ port Quantum Alice ]
      Outputs = [ port Quantum Alice; port Quantum Bob ]
      Gate = gate_copy }

let cobit_BA =
    { Name = "cobit_BA"
      Inputs = [ port Quantum Bob ]
      Outputs = [ port Quantum Bob; port Quantum Alice ]
      Gate = gate_copy }

let CopyCbit =
    { Name = "cbit"
      Inputs = [ port Classical Any ]
      Outputs =
          [ port Classical Any
            port Classical Any ]
      Gate = gate_copy }


let gate_not: Gate =
    function
    | [ in1 ], [ out1 ] -> map (modifyBit in1 (fun val1 -> B [ out1, not val1 ]))
    | _ -> failwith "wires not correct"

let X =
    { Name = "X"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Gate = gate_not }

let Not =
    { Name = "Not"
      Inputs = [ port Classical Any ]
      Outputs = [ port Classical Any ]
      Gate = gate_not }


let gate_Z: Gate =
    function
    | [ in1 ], [ out1 ] -> unitary1 in1 (fun val1 -> Ket [ out1, val1 ] * (if val1 then -1.0 else 1.0))
    | _ -> failwith "wires not correct"

let Z =
    { Name = "Z"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Gate = gate_Z }


let gate_H: Gate =
    function
    | [ in1 ], [ out1 ] ->
        unitary1 in1 (fun val1 ->
            (Ket [ out1, false ]
             + Ket [ out1, true ]
             * (if val1 then -1.0 else 1.0))
            / sqrt 2.0)
    | _ -> failwith "wires not correct"

let H =
    { Name = "H"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Gate = gate_H }


let gate_CNOT: Gate =
    function
    | [ in1; in2 ], [ out1; out2 ] ->
        map
            (modifyBits [ in1; in2 ] (fun [ val1; val2 ] ->
                 B [ out1, val1
                     out2, (if val1 then not val2 else val2) ]))
    | _ -> failwith "wires not correct"

let CNOT =
    { Name = "CNOT"
      Inputs = [ port Quantum Any; port Quantum Any ]
      Outputs = [ port Quantum Any; port Quantum Any ]
      Gate = gate_CNOT }

let CNOT_AB =
    { Name = "CNOT_AB"
      Inputs = [ port Quantum Alice; port Quantum Bob ]
      Outputs = [ port Quantum Alice; port Quantum Bob ]
      Gate = gate_CNOT }

let CNOT_BA =
    { Name = "CNOT_BA"
      Inputs = [ port Quantum Bob; port Quantum Alice ]
      Outputs = [ port Quantum Bob; port Quantum Alice ]
      Gate = gate_CNOT }


let gate_SWAP: Gate =
    function
    | [ in1; in2 ], [ out1; out2 ] -> map (modifyBits [ in1; in2 ] (fun [ val1; val2 ] -> B [ out1, val2; out2, val1 ]))
    | _ -> failwith "wires not correct"

let SWAP =
    { Name = "SWAP"
      Inputs = [ port Quantum Alice; port Quantum Bob ]
      Outputs = [ port Quantum Alice; port Quantum Bob ]
      Gate = gate_SWAP }


let gate_M: Gate =
    function
    | [ in1 ], [ out1 ] ->
        SparseVector.mapBoth (fun ((b1, b2), v) -> (b1, b2), (if read b1 in1 = read b2 in1 then v else Complex.zero))
        >> map (copyBits in1 [ out1 ])
    | _ -> failwith "wires not correct"

let M =
    { Name = "M"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Classical Any ]
      Gate = gate_M }


let gate_InitBit: Gate =
    function
    | [], [ out1 ] -> map (merge (B [ out1, false ]))
    | _ -> failwith "wires not correct"

let InitCbit =
    { Name = "Init Cbit"
      Inputs = []
      Outputs = [ port Classical Any ]
      Gate = gate_InitBit }

let InitQubit =
    { Name = "Init Qubit"
      Inputs = []
      Outputs = [ port Quantum Any ]
      Gate = gate_InitBit }


let gate_InitQubitRandom: Gate =
    function
    | [], [ out1 ] ->
        unitary [] (fun [] ->
            let a0 =
                Complex(myRandom.NextDouble(), myRandom.NextDouble())

            let a1 =
                Complex(myRandom.NextDouble(), myRandom.NextDouble())

            let norm =
                sqrt (a0.Magnitude ** 2.0 + a1.Magnitude ** 2.0)

            (a0 * Ket [ out1, false ] + a1 * Ket [ out1, true ])
            / norm)
    | _ -> failwith "wires not correct"

let InitQubitRandom =
    { Name = "Init Random Qubit"
      Inputs = []
      Outputs = [ port Quantum Any ]
      Gate = gate_InitQubitRandom }


let gate_InitCbitRandom: Gate =
    function
    | [], [ out1 ] -> map (merge (B [ out1, myRandom.NextDouble() > 0.5 ]))
    | _ -> failwith "wires not correct"

let InitCbitRandom =
    { Name = "Init Random Cbit"
      Inputs = []
      Outputs = [ port Classical Any ]
      Gate = gate_InitCbitRandom }


let gate_DestroyBit: Gate =
    function
    | [ in1 ], [] ->
        SparseVector.mapBoth (fun ((b1, b2), v) -> (b1, b2), (if read b1 in1 = read b2 in1 then v else Complex.zero))
        >> map (removeAll [ in1 ])
    | _ -> failwith "wires not correct"

let DestroyCbit =
    { Name = "Destroy Cbit"
      Inputs = [ port Classical Any ]
      Outputs = []
      Gate = gate_DestroyBit }

let DestroyQubit =
    { Name = "Destroy Qubit"
      Inputs = [ port Quantum Any ]
      Outputs = []
      Gate = gate_DestroyBit }
