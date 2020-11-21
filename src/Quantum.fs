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


type PureState = PureState of SparseVector<Bits>
type MixedState = MixedState of SparseVector<Bits * Bits>
// type Channel = Bits * Bits -> MixedState


let Ket x =
    SparseVector.ofSeq [ x, Complex.one ] |> PureState



let outer (PureState ket) (PureState bra) =
    SparseVector.tensor ket (SparseVector.map (fun (k, v) -> k, Complex.conjugate v) bra)
    |> MixedState

let adjoint (MixedState rho) =
    rho
    |> SparseVector.map (fun ((b1, b2), v) -> (b2, b1), Complex.conjugate v)
    |> MixedState

let trace (MixedState rho) =
    rho
    |> SparseVector.sumBy (fun (b1, b2) -> if b1 = b2 then Complex.one else Complex.zero)
