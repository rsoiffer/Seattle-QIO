module App

open Browser.Dom
open Quantum
open BoardState


let boardState =
    { Nodes =
          Map.ofSeq [ NodeId 1,
                      { Name = "Allocate"
                        Position = (0.0, 0.0)
                        NodeType =
                            { Inputs = []
                              Outputs = [ { DataType = Quantum; Party = Alice } ] }
                        Implementation = GateImplementations.InitQubit }
                      NodeId 2,
                      { Name = "X"
                        Position = (0.0, 0.0)
                        NodeType =
                            { Inputs = [ { DataType = Quantum; Party = Alice } ]
                              Outputs = [ { DataType = Quantum; Party = Alice } ] }
                        Implementation = GateImplementations.H } ]
      Wires =
          Map.ofSeq [ WireId 3,
                      { WireType = { DataType = Quantum; Party = Alice }
                        Placement =
                            { Output = { NodeId = NodeId 1; Port = 0 }
                              Input = Some { NodeId = NodeId 2; Port = 0 } } }
                      WireId 4,
                      { WireType = { DataType = Quantum; Party = Alice }
                        Placement =
                            { Output = { NodeId = NodeId 2; Port = 0 }
                              Input = None } } ] }

let evalState = Evaluator.eval boardState

// Mutable variable to count the number of times we clicked the button
let mutable count = 0

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton =
    document.querySelector (".my-button") :?> Browser.Types.HTMLButtonElement

// Register our listener
myButton.onclick <-
    fun _ ->
        count <- add1 count
        myButton.innerText <- evalState.ToString()
