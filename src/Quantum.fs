module Quantum

#nowarn "25"

open ComplexNumbers


type WireId = WireId of int

type Bits = Bits of Map<WireId, bool>

let removeAll wires (Bits bits) =
    Seq.fold (fun b w -> Map.remove w b) bits wires
    |> Bits

let merge (Bits newBits) (Bits bits) =
    newBits
    |> Seq.fold (fun b nb -> Map.add nb.Key nb.Value b) bits
    |> Bits

let read (Bits bits) wireId = bits.[wireId]

let B x = x |> Map.ofSeq |> Bits

type Component = { Amplitude: Complex; State: Bits }
let getAmplitude c = c.Amplitude
let getBits c = c.State

type Qubits = Qubits of Component list


module QubitInternals =

    let simplify (Qubits q) =
        seq {
            for b, q in Seq.groupBy getBits q do
                let mySum =
                    q
                    |> Seq.map getAmplitude
                    |> Seq.fold (+) Complex.Zero

                if mySum.Magnitude > 1e-6 then yield { Amplitude = mySum; State = b }
        }
        |> List.ofSeq
        |> Qubits

    let make = List.ofSeq >> Qubits >> simplify

open QubitInternals


type Qubits with

    static member Zero = Qubits List.empty

    static member (+)(Qubits q1, Qubits q2) = Seq.concat [ q1; q2 ] |> make

    static member (-)(q1: Qubits, q2: Qubits) = q1 + q2 * -Complex.One

    static member (*)(Qubits q, mult: Complex) =
        q
        |> Seq.map (fun c ->
            { c with
                  Amplitude = c.Amplitude * mult })
        |> make

    static member (*)(q: Qubits, mult: float) = q * Complex(mult, 0.0)

    static member (*)(mult: Complex, q: Qubits) = q * mult

    static member (*)(mult: float, q: Qubits) = q * mult

    static member (/)(q: Qubits, mult: Complex) = q * (Complex.One / mult)

    static member (/)(q: Qubits, mult: float) = q * (1.0 / mult)


module Qubits =

    let Pure bits =
        Qubits [ { Amplitude = Complex.One
                   State = bits } ]

    let Ket = B >> Pure

    let magnitude (Qubits qubits) =
        Seq.sumBy (fun c -> c.Amplitude.Magnitude ** 2.0) qubits

    let postSelect wireId val' (Qubits qubits) =
        qubits
        |> Seq.filter (fun c -> read c.State wireId = val')
        |> make

    let prob wireId val' qubits =
        postSelect wireId val' qubits |> magnitude

    let normalize qubits = qubits / sqrt (magnitude qubits)

open Qubits


type SystemState =
    { ClassicalState: Bits
      QuantumState: Qubits }

let emptyState =
    { ClassicalState = B []
      QuantumState = Ket [] }

let modifyClassical f state =
    { state with
          ClassicalState = f state.ClassicalState }

let modifyQuantum f state =
    { state with
          QuantumState = f state.QuantumState }



module GateImplementations =
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


    type GateImplementation = (WireId list) * (WireId list) -> SystemState -> SystemState

    let qbit: GateImplementation =
        function
        | [ in1 ], [ out1 ] -> mapQuantum (copyBits in1 [ out1 ])
        | _ -> failwith "wires not correct"

    let cobit: GateImplementation =
        function
        | [ in1 ], [ out1; out2 ] -> mapQuantum (copyBits in1 [ out1; out2 ])
        | _ -> failwith "wires not correct"

    let cbit: GateImplementation =
        function
        | [ in1 ], [ out1 ] -> modifyClassical (copyBits in1 [ out1 ])
        | _ -> failwith "wires not correct"

    let X: GateImplementation =
        function
        | [ in1 ], [ out1 ] -> modifyQuantum (modifyQubit in1 (fun val1 -> Ket [ out1, not val1 ]))
        | _ -> failwith "wires not correct"

    let Z: GateImplementation =
        function
        | [ in1 ], [ out1 ] ->
            modifyQuantum
                (modifyQubit in1 (fun val1 ->
                     (Ket [ out1, val1 ])
                     * (if val1 then -1.0 else 1.0)))
        | _ -> failwith "wires not correct"

    let H: GateImplementation =
        function
        | [ in1 ], [ out1 ] ->
            modifyQuantum
                (modifyQubit in1 (fun val1 ->
                     (Ket [ out1, false ]
                      + Ket [ out1, true ]
                      * (if val1 then -1.0 else 1.0))
                     / sqrt 2.0))
        | _ -> failwith "wires not correct"

    let myRandom = System.Random()

    let M: GateImplementation =
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

    let InitQubit: GateImplementation =
        function
        | [], [ out1 ] -> modifyQuantum (modifyQubits [] (fun [] -> Ket [ out1, false ]))
        | _ -> failwith "wires not correct"

    let InitCbit: GateImplementation =
        function
        | [], [ out1 ] -> modifyClassical (merge (B [ out1, false ]))
        | _ -> failwith "wires not correct"

    let DestroyCbit: GateImplementation =
        function
        | [ in1 ], [] -> modifyClassical (removeAll [ in1 ])
        | _ -> failwith "wires not correct"





let add1 x = x + 1
