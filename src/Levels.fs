module Levels

open Quantum
open Gates
open Circuit
open Board

type Challenge =
    { Free: NodeDefinition
      Costly: (NodeDefinition * int) list
      Goal: NodeDefinition }


let initialBoard challenge =
    let myRandom = System.Random()

    let inputId = myRandom.Next()
    let outputId = myRandom.Next()
    let oracleId = myRandom.Next()

    let inputCreatorIds =
        challenge.Goal.Inputs
        |> Seq.map (fun _ -> myRandom.Next())

    let inputCopierIds =
        challenge.Goal.Inputs
        |> Seq.map (fun _ -> myRandom.Next())

    let outputCnotIds =
        challenge.Goal.Outputs
        |> Seq.map (fun _ -> myRandom.Next())

    let outputDestructorIds =
        challenge.Goal.Outputs
        |> Seq.map (fun _ -> myRandom.Next())

    let allNodes =
        seq {
            let inputNode =
                { Definition =
                      { Name = "Input"
                        Inputs = challenge.Goal.Inputs
                        Outputs = challenge.Goal.Inputs
                        Gate = gate_DoNothing }
                  Visibility = HideInputs }

            let outputNode =
                { Definition =
                      { Name = "Output"
                        Inputs = challenge.Goal.Outputs
                        Outputs = challenge.Goal.Outputs
                        Gate = gate_DoNothing }
                  Visibility = HideInputs }

            let oracleNode =
                { Definition = challenge.Goal
                  Visibility = Invisible }

            yield inputId, inputNode
            yield outputId, outputNode
            yield oracleId, oracleNode

            for inputPort, creatorId, copierId in Seq.zip3 challenge.Goal.Inputs inputCreatorIds inputCopierIds do
                match inputPort.DataType with
                | Classical ->
                    let creatorNode =
                        { Definition = InitCbitRandom
                          Visibility = Invisible }

                    let copierNode =
                        { Definition = CopyCbit
                          Visibility = Invisible }

                    yield creatorId, creatorNode

                    yield copierId, copierNode
                | Quantum ->
                    let creatorNode =
                        { Definition = InitQubitRandom
                          Visibility = Invisible }

                    let copierNode =
                        { Definition = cobit
                          Visibility = Invisible }

                    yield creatorId, creatorNode

                    yield copierId, copierNode

            for outputPort, cnotId, destructorId in Seq.zip3 challenge.Goal.Outputs outputCnotIds outputDestructorIds do
                match outputPort.DataType with
                | Classical -> ()
                | Quantum -> ()
        }


    { Nodes = Map.ofSeq []
      Wires = Map.ofSeq [] }
