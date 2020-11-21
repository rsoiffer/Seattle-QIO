module Quantum

open ComplexNumbers
open SparseVector


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


type PureState = SparseVector<Bits>
type MixedState = SparseVector<Bits * Bits>


let outer ket bra =
    SparseVector.tensor ket (SparseVector.mapWeights Complex.conjugate bra)

let adjoint rho =
    rho
    |> SparseVector.mapBoth (fun ((b1, b2), v) -> (b2, b1), Complex.conjugate v)

let trace rho =
    rho
    |> SparseVector.sumBy (fun (b1, b2) -> if b1 = b2 then Complex.one else Complex.zero)

let Ket x = SparseVector.ofSeq [ B x, Complex.one ]
let Rho x = outer (Ket x) (Ket x)
