module Levels

open Quantum
open Gates
open Circuit

type Challenge =
    { Free: NodeDefinition
      Costly: (NodeDefinition * int) list
      Goal: NodeDefinition }

let initialBoard challenge =
    let goal = challenge.Goal
    // let inputs = goal.Inputs |> Seq.map (function
    // | Classical -> {
    //     Definition = Ini
    // }
    // )
    { Nodes = Map.ofSeq []
      Wires = Map.ofSeq [] }


let testOnce boardState successWire =
    read (eval boardState).State.ClassicalState successWire

let test boardState successWire numTrials =
    let successes =
        seq { 1 .. numTrials }
        |> Seq.map (fun i -> testOnce boardState successWire)
        |> Seq.sumBy (fun b -> if b then 1 else 0)

    float successes / float numTrials
