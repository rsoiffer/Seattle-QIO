module Quantum


let simplify (data: ('A * float) seq) =
    seq {
        for a, l in Seq.groupBy (fun (a, f) -> a) data do
            let mySum = Seq.sumBy (fun (a, f) -> f) l
            if abs (mySum) > 1e-6 then yield a, mySum
    }

type QuantumState<'A when 'A: comparison>(data: ('A * float) seq) =
    member this.Data = data |> simplify

    static member Zero = QuantumState Seq.empty

    static member inline (+)(q1: QuantumState<'A>, q2: QuantumState<'A>) =
        Seq.concat [ q1.Data; q2.Data ] |> QuantumState

    static member inline (-)(q1: QuantumState<'A>, q2: QuantumState<'A>) = q1 + q2 * -1.0

    static member inline (*)(q1: QuantumState<'A>, q2: QuantumState<'B>) =
        seq {
            for (a1, f1) in q1.Data do
                for (a2, f2) in q2.Data do
                    yield (a1, a2), f1 * f2
        }
        |> QuantumState

    static member inline (*)(q: QuantumState<'A>, mult: float) =
        seq {
            for (a, f) in q.Data do
                yield a, f * mult
        }
        |> QuantumState

    static member inline (/)(q: QuantumState<'A>, mult: float) = q * (1.0 / mult)

    member this.ApplyQ(func: 'A -> QuantumState<'B>) =
        seq {
            for (a, f) in this.Data do
                yield func a * f
        }
        |> Seq.sum

    member this.Apply(func: 'A -> 'B) =
        seq {
            for (a, f) in this.Data do
                yield func a, f
        }
        |> QuantumState

let Pure a = QuantumState [ (a, 1.0) ]


type WireID = int
type Bits = Map<WireID, bool>
type Qubits = QuantumState<Bits>

let Qubits (bitList: (WireID * bool) list) = Pure(Map.ofSeq bitList)

let modifyBits ins f (bits: Bits) =
    let result =
        List.map (fun in' -> Map.find in' bits) ins |> f

    let mutable bits = bits
    for in' in ins do
        bits <- bits.Remove in'
    for res in result do
        bits <- bits.Add res
    bits

let modifyQubits ins (f: bool list -> Qubits) (qubits: Qubits) =
    qubits.ApplyQ(fun bits ->
        let result =
            List.map (fun in' -> Map.find in' bits) ins |> f

        let mutable bits = bits
        for in' in ins do
            bits <- bits.Remove in'

        result.Apply(fun mods ->
            for res in mods do
                bits <- bits.Add(res.Key, res.Value)
            bits))

let modifyQubit in' (f: bool -> Qubits) (qubits: Qubits) =
    modifyQubits [ in' ] (function
        | [ val' ] -> f val') qubits

let copyBits in' outs (bits: Bits) =
    let inVal = bits.[in']
    let mutable bits = bits.Remove in'
    for out' in outs do
        bits <- bits.Add(out', inVal)
    bits



type SystemState =
    { ClassicalState: Bits
      QuantumState: Qubits }

let applyQuantum f state =
    { state with
          QuantumState = state.QuantumState.Apply f }

let applyQuantumQ f state =
    { state with
          QuantumState = state.QuantumState.ApplyQ f }

let modifyQuantum f state =
    { state with
          QuantumState = f state.QuantumState }

let modifyClassical f state =
    { state with
          ClassicalState = f state.ClassicalState }

type GateImplementation = (WireID list) * (WireID list) -> SystemState -> SystemState

let qbit: GateImplementation =
    function
    | [ in1 ], [ out1 ] -> applyQuantum (copyBits in1 [ out1 ])
    | _ -> failwith "wires not correct"

let cobit: GateImplementation =
    function
    | [ in1 ], [ out1; out2 ] -> applyQuantum (copyBits in1 [ out1; out2 ])
    | _ -> failwith "wires not correct"

let cbit: GateImplementation =
    function
    | [ in1 ], [ out1 ] -> modifyClassical (copyBits in1 [ out1 ])
    | _ -> failwith "wires not correct"

let X: GateImplementation =
    function
    | [ in1 ], [ out1 ] -> modifyQuantum (modifyQubit in1 (fun val1 -> Qubits [ out1, not val1 ]))
    | _ -> failwith "wires not correct"

let Z: GateImplementation =
    function
    | [ in1 ], [ out1 ] ->
        modifyQuantum
            (modifyQubit in1 (fun val1 ->
                 (Qubits [ out1, val1 ])
                 * (if val1 then -1.0 else 1.0)))
    | _ -> failwith "wires not correct"

let H: GateImplementation =
    function
    | [ in1 ], [ out1 ] ->
        modifyQuantum
            (modifyQubit in1 (fun val1 ->
                 (Qubits [ out1, false ]
                  + Qubits [ out1, true ]
                  * (if val1 then -1.0 else 1.0))
                 / sqrt 2.0))
    | _ -> failwith "wires not correct"




let add1 x = x + 1
