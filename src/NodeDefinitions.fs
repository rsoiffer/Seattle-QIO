module NodeDefinitions

open Quantum

type DataType =
    | Classical
    | Quantum

type Party =
    | Any
    | Alice
    | Bob

type Port =
    { DataType: DataType
      Party: Party }

let Port d p = { DataType = d; Party = p }

type Node =
    { Name: string
      Inputs: Port list
      Outputs: Port list
      Implementation: GateImplementations.GateImplementation }


let X =
    { Name = "X"
      Inputs = [ Port Quantum Any ]
      Outputs = [ Port Quantum Any ]
      Implementation = GateImplementations.X }

let Z =
    { Name = "Z"
      Inputs = [ Port Quantum Any ]
      Outputs = [ Port Quantum Any ]
      Implementation = GateImplementations.Z }
