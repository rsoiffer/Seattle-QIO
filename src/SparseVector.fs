module SparseVector

open ComplexNumbers

type SparseVector<'a when 'a: comparison> = SparseVector of Map<'a, Complex>

let read k v =
    Option.defaultValue Complex.zero (Map.tryFind k v)

type SparseVector<'a> with

    static member Zero = SparseVector Map.empty

    static member (+)(SparseVector s1, SparseVector s2) =
        seq {
            let keys =
                [ Map.toSeq s1; Map.toSeq s2 ]
                |> Seq.concat
                |> Seq.map fst
                |> Set.ofSeq

            for k in keys do
                let v = read k s1 + read k s2
                if Complex.abs v > 1e-12 then yield k, v
        }
        |> Map.ofSeq
        |> SparseVector

    static member (-)(s1: SparseVector<'a>, s2: SparseVector<'a>) = s1 + s2 * -Complex.one

    static member (*)(SparseVector s, mult: Complex) =
        Map.map (fun _ v -> v * mult) s |> SparseVector

    static member (*)(mult: Complex, s: SparseVector<'a>) = s * mult

    static member (*)(s: SparseVector<'a>, mult: float) = s * Complex(mult, 0.0)

    static member (*)(mult: float, s: SparseVector<'a>) = s * mult

    static member (/)(s: SparseVector<'a>, mult: float) = s * (1.0 / mult)

    static member (/)(s: SparseVector<'a>, mult: Complex) = s * Complex.div Complex.one mult

let apply (channel: 'a -> SparseVector<_>) (SparseVector s) =
    s
    |> Map.toSeq
    |> Seq.sumBy (fun (k, v) -> v * channel k)
