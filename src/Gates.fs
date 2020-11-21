module Gates

#nowarn "25" "49"

open ComplexNumbers
open Quantum
open QubitInternals
open Qubits

module GateHelpers =
    let bind (func: Bits -> Qubits) (Qubits q) =
        seq {
            for c in q do
                yield c.Amplitude * func c.State
        }
        |> Seq.sum

    let map (func: Bits -> Bits) (Qubits q) =
        seq {
            for c in q do
                yield { c with State = func c.State }
        }
        |> make

    let mapQuantum f = modifyQuantum (map f)

    let bindQuantum f = modifyQuantum (bind f)

    let modifyBits ins f bits =
        let result = List.map (read bits) ins |> f
        bits |> removeAll ins |> merge result

    let modifyQubits ins f =
        bind (fun bits ->
            List.map (read bits) ins
            |> f
            |> map (fun mods -> bits |> removeAll ins |> merge mods))

    let modifyQubit in' f =
        modifyQubits [ in' ] (fun [ val' ] -> f val')

    let copyBits in' outs =
        modifyBits [ in' ] (fun [ val' ] -> outs |> Seq.map (fun out' -> out', val') |> B)

    let myRandom = System.Random()

open GateHelpers



type Gate = WireId list * WireId list -> SystemState -> SystemState

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


let gate_qbit: Gate =
    function
    | [ in1 ], [ out1 ] -> mapQuantum (copyBits in1 [ out1 ])
    | _ -> failwith "wires not correct"

let qbit =
    { Name = "qbit"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Gate = gate_qbit }

let qbit_AB =
    { Name = "qbit_AB"
      Inputs = [ port Quantum Alice ]
      Outputs = [ port Quantum Bob ]
      Gate = gate_qbit }

let qbit_BA =
    { Name = "qbit_BA"
      Inputs = [ port Quantum Bob ]
      Outputs = [ port Quantum Alice ]
      Gate = gate_qbit }


let gate_cobit: Gate =
    function
    | [ in1 ], [ out1; out2 ] -> mapQuantum (copyBits in1 [ out1; out2 ])
    | _ -> failwith "wires not correct"

let cobit =
    { Name = "cobit"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any; port Quantum Any ]
      Gate = gate_cobit }

let cobit_AB =
    { Name = "cobit_AB"
      Inputs = [ port Quantum Alice ]
      Outputs = [ port Quantum Alice; port Quantum Bob ]
      Gate = gate_cobit }

let cobit_BA =
    { Name = "cobit_BA"
      Inputs = [ port Quantum Bob ]
      Outputs = [ port Quantum Bob; port Quantum Alice ]
      Gate = gate_cobit }


let gate_cbit: Gate =
    function
    | [ in1 ], [ out1 ] -> modifyClassical (copyBits in1 [ out1 ])
    | _ -> failwith "wires not correct"

let cbit =
    { Name = "cbit"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Gate = gate_cbit }

let cbit_AB =
    { Name = "cbit_AB"
      Inputs = [ port Quantum Alice ]
      Outputs = [ port Quantum Bob ]
      Gate = gate_cbit }

let cbit_BA =
    { Name = "cbit_BA"
      Inputs = [ port Quantum Bob ]
      Outputs = [ port Quantum Alice ]
      Gate = gate_cbit }


let gate_CopyCbit: Gate =
    function
    | [ in1 ], [ out1; out2 ] -> modifyClassical (copyBits in1 [ out1; out2 ])
    | _ -> failwith "wires not correct"

let CopyCbit =
    { Name = "cbit"
      Inputs = [ port Classical Any ]
      Outputs =
          [ port Classical Any
            port Classical Any ]
      Gate = gate_cbit }


let gate_X: Gate =
    function
    | [ in1 ], [ out1 ] -> modifyQuantum (modifyQubit in1 (fun val1 -> Ket [ out1, not val1 ]))
    | _ -> failwith "wires not correct"

let X =
    { Name = "X"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Gate = gate_X }


let gate_Z: Gate =
    function
    | [ in1 ], [ out1 ] ->
        modifyQuantum
            (modifyQubit in1 (fun val1 ->
                 (Ket [ out1, val1 ])
                 * (if val1 then -1.0 else 1.0)))
    | _ -> failwith "wires not correct"

let Z =
    { Name = "Z"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Gate = gate_Z }


let gate_H: Gate =
    function
    | [ in1 ], [ out1 ] ->
        modifyQuantum
            (modifyQubit in1 (fun val1 ->
                 (Ket [ out1, false ]
                  + Ket [ out1, true ]
                  * (if val1 then -1.0 else 1.0))
                 / sqrt 2.0))
    | _ -> failwith "wires not correct"

let H =
    { Name = "H"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Gate = gate_H }


let gate_CNOT: Gate =
    function
    | [ in1; in2 ], [ out1; out2 ] ->
        modifyQuantum
            (modifyQubits [ in1; in2 ] (fun [ val1; val2 ] ->
                 Ket [ out1, val1
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
    | [ in1; in2 ], [ out1; out2 ] ->
        modifyQuantum (modifyQubits [ in1; in2 ] (fun [ val1; val2 ] -> Ket [ out1, val2; out2, val1 ]))
    | _ -> failwith "wires not correct"

let SWAP =
    { Name = "SWAP"
      Inputs = [ port Quantum Alice; port Quantum Bob ]
      Outputs = [ port Quantum Alice; port Quantum Bob ]
      Gate = gate_SWAP }


let gate_M: Gate =
    function
    | [ in1 ], [ out1 ] ->
        fun state ->
            let val' =
                myRandom.NextDouble() < prob in1 true state.QuantumState

            { ClassicalState = merge (B [ out1, val' ]) state.ClassicalState
              QuantumState =
                  postSelect in1 val' state.QuantumState
                  |> map (removeAll [ in1 ])
                  |> normalize }
    | _ -> failwith "wires not correct"

let M =
    { Name = "M"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Classical Any ]
      Gate = gate_M }


let gate_InitQubit: Gate =
    function
    | [], [ out1 ] -> modifyQuantum (modifyQubits [] (fun [] -> Ket [ out1, false ]))
    | _ -> failwith "wires not correct"

let InitQubit =
    { Name = "Init Qubit"
      Inputs = []
      Outputs = [ port Quantum Any ]
      Gate = gate_InitQubit }


let gate_InitQubitRandom: Gate =
    function
    | [], [ out1 ] ->
        modifyQuantum
            (modifyQubits [] (fun [] ->
                 let a0 =
                     Complex(myRandom.NextDouble(), myRandom.NextDouble())

                 let a1 =
                     Complex(myRandom.NextDouble(), myRandom.NextDouble())

                 let norm =
                     sqrt (a0.Magnitude ** 2.0 + a1.Magnitude ** 2.0)

                 (a0 * Ket [ out1, false ] + a1 * Ket [ out1, true ])
                 / norm))
    | _ -> failwith "wires not correct"

let InitQubitRandom =
    { Name = "Init Random Qubit"
      Inputs = []
      Outputs = [ port Quantum Any ]
      Gate = gate_InitQubitRandom }


let gate_InitCbit: Gate =
    function
    | [], [ out1 ] -> modifyClassical (merge (B [ out1, false ]))
    | _ -> failwith "wires not correct"

let InitCbit =
    { Name = "Init Cbit"
      Inputs = []
      Outputs = [ port Classical Any ]
      Gate = gate_InitCbit }


let gate_InitCbitRandom: Gate =
    function
    | [], [ out1 ] -> modifyClassical (merge (B [ out1, myRandom.NextDouble() > 0.5 ]))
    | _ -> failwith "wires not correct"

let InitCbitRandom =
    { Name = "Init Random Cbit"
      Inputs = []
      Outputs = [ port Classical Any ]
      Gate = gate_InitCbitRandom }


let gate_DestroyCbit: Gate =
    function
    | [ in1 ], [] -> modifyClassical (removeAll [ in1 ])
    | _ -> failwith "wires not correct"

let DestroyCbit =
    { Name = "Destroy Cbit"
      Inputs = [ port Classical Any ]
      Outputs = []
      Gate = gate_DestroyCbit }


let gate_DestroyQubit: Gate =
    function
    | [ in1 ], [] ->
        modifyQuantum (fun qState ->
            let val' =
                myRandom.NextDouble() < prob in1 true qState

            postSelect in1 val' qState
            |> map (removeAll [ in1 ])
            |> normalize)
    | _ -> failwith "wires not correct"

let DestroyQubit =
    { Name = "Destroy Qubit"
      Inputs = [ port Quantum Any ]
      Outputs = []
      Gate = gate_DestroyQubit }
