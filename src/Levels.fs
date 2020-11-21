module Levels

open Quantum
open Circuit
open NodeDefinitions

type Challenge =
    { Free: NodeDefinition
      Costly: (NodeDefinition * int) list
      Goal: NodeDefinition }


let testOnce boardState successWire =
    read (eval boardState).State.ClassicalState successWire

let test boardState successWire numTrials =
    let successes =
        seq { 1 .. numTrials }
        |> Seq.map (fun i -> testOnce boardState successWire)
        |> Seq.sumBy (fun b -> if b then 1 else 0)

    float successes / float numTrials
