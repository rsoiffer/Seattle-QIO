namespace SeattleQio.Simulator

type SparseVector<'a when 'a: comparison> = private SparseVector of Map<'a, Complex>

module SparseVector =
    let read k (SparseVector s) =
        Option.defaultValue Complex.zero (Map.tryFind k s)

    let ofSeq s =
        s
        |> Seq.fold (fun m (k, v) ->
            let v2 =
                match Map.tryFind k m with
                | Some v' -> v + v'
                | None -> v

            if Complex.abs v2 > 1e-12 then Map.add k v2 m else Map.remove k m) Map.empty
        |> SparseVector

    let toSeq (SparseVector s) = s |> Map.toSeq

    let ofSeqF s =
        s
        |> Seq.map (fun (k, v) -> k, Complex(v, 0.0))
        |> ofSeq

    let mapBoth (f: _ -> _) = toSeq >> Seq.map f >> ofSeq

    let map (f: _ -> _) = mapBoth (fun (k, v) -> f k, v)

    let mapWeights (f: _ -> _) = mapBoth (fun (k, v) -> k, f v)

    let zero = SparseVector Map.empty

    let sum s1 s2 =
        Seq.concat [ toSeq s1; toSeq s2 ] |> ofSeq

    let mul mult = mapWeights (fun v -> v * mult)

    let bind (channel: _ -> SparseVector<_>) =
        toSeq
        >> Seq.map (fun (k, v) -> mul v (channel k))
        >> Seq.fold sum zero

    let sumBy (f: _ -> Complex) =
        toSeq >> Seq.sumBy (fun (k, v) -> v * f k)

    let tensor s1 s2 =
        seq {
            for k1, v1 in toSeq s1 do
                for k2, v2 in toSeq s2 do
                    yield (k1, k2), v1 * v2
        }
        |> ofSeq

    let approximately epsilon (SparseVector s1) (SparseVector s2) =
        let keys =
            Map.toSeq s1
            |> Seq.map fst
            |> Seq.append (Map.toSeq s2 |> Seq.map fst)
            |> Set.ofSeq

        keys
        |> Set.forall (fun key ->
            match Map.tryFind key s1, Map.tryFind key s2 with
            | Some value1, Some value2 -> Complex.abs (value1 - value2) <= epsilon
            | _ -> false)

open SparseVector

type SparseVector<'c when 'c: comparison> with
    static member Zero = zero

    static member (+)(s1: SparseVector<'a>, s2: SparseVector<'a>) = sum s1 s2

    static member (-)(s1: SparseVector<'a>, s2: SparseVector<'a>) = s1 + s2 * -1.0

    static member (*)(s: SparseVector<'a>, mult: Complex) = mul mult s

    static member (*)(mult: Complex, s: SparseVector<'a>) = s * mult

    static member (*)(s: SparseVector<'a>, mult: float) = mul (Complex(mult, 0.0)) s

    static member (*)(mult: float, s: SparseVector<'a>) = mul (Complex(mult, 0.0)) s

    static member (/)(s: SparseVector<'a>, mult: float) = s * (1.0 / mult)

    static member (/)(s: SparseVector<'a>, mult: Complex) = mul (Complex.div Complex.one mult) s
