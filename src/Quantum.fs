module Quantum

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
