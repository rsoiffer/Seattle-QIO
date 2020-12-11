﻿namespace SeattleQio.Editor.Collections

module internal Seq =
    let tryMax xs =
        let xs = Seq.cache xs
        if Seq.isEmpty xs then None else Seq.max xs |> Some

module internal Map =
    let change key f map =
        match Map.tryFind key map |> f with
        | Some value -> Map.add key value map
        | None -> map
