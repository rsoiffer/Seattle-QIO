module SeattleQio.Simulator.Quantum

type WireId = WireId of int

type Bits = Bits of Map<WireId, bool>

let removeAll wires (Bits bits) =
    for w in wires do
        if not (Map.containsKey w bits) then failwith "Removing invalid wire"

    Seq.fold (fun b w -> Map.remove w b) bits wires
    |> Bits

let merge (Bits newBits) (Bits bits) =
    newBits
    |> Seq.fold (fun b nb -> Map.add nb.Key nb.Value b) bits
    |> Bits

let read (Bits bits) wireId = bits.[wireId]

let allWires (Bits bits) = bits |> Map.toSeq |> Seq.map fst

let B x = x |> Map.ofSeq |> Bits

let rec allPossibleBits =
    function
    | [] -> seq { B [] }
    | head :: tail ->
        seq {
            for Bits others in allPossibleBits tail do
                yield Map.add head false others |> Bits
                yield Map.add head true others |> Bits
        }

type PureState = SparseVector<Bits>

type MixedState = SparseVector<Bits * Bits>

let outer ket bra =
    SparseVector.tensor ket (SparseVector.mapWeights Complex.conjugate bra)

let adjoint rho =
    rho
    |> SparseVector.mapBoth (fun ((b1, b2), v) -> (b2, b1), Complex.conjugate v)

let norm psi =
    psi
    |> SparseVector.mapBoth (fun (k, v) -> v, Complex.one)
    |> SparseVector.sumBy (fun v -> Complex(v.Magnitude ** 2.0, 0.0))

let trace rho =
    rho
    |> SparseVector.sumBy (fun (b1, b2) -> if b1 = b2 then Complex.one else Complex.zero)

let Ket x = SparseVector.ofSeq [ B x, Complex.one ]

let Rho x = outer (Ket x) (Ket x)

let prettyPrint (rho: MixedState) =
    let wires =
        rho
        |> SparseVector.toSeq
        |> Seq.head
        |> fst
        |> fst
        |> allWires
        |> List.ofSeq

    let allBits = allPossibleBits wires |> List.ofSeq

    let entries =
        allBits
        |> List.map (fun x ->
            allBits
            |> List.map (fun y ->
                let a = SparseVector.read (x, y) rho
                if (a.Magnitude < 1e-6) then "              0" else sprintf "(%.2f + %.2f i)" a.r a.i))

    entries
    |> Seq.map (Seq.fold (fun x y -> x + "  " + y) "")
    |> Seq.fold (fun x y -> x + "\n" + y) ""
