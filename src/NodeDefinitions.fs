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

type NodeDefinition =
    { Name: string
      Inputs: Port list
      Outputs: Port list
      Implementation: Gates.Gate }


let X =
    { Name = "X"
      Inputs = [ Port Quantum Any ]
      Outputs = [ Port Quantum Any ]
      Implementation = Gates.X }

let Z =
    { Name = "Z"
      Inputs = [ Port Quantum Any ]
      Outputs = [ Port Quantum Any ]
      Implementation = Gates.Z }

let H =
    { Name = "H"
      Inputs = [ Port Quantum Any ]
      Outputs = [ Port Quantum Any ]
      Implementation = Gates.H }

let qbit_AB =
    { Name = "qbit_AB"
      Inputs = [ Port Quantum Alice ]
      Outputs = [ Port Quantum Bob ]
      Implementation = Gates.qbit }

let qbit_BA =
    { Name = "qbit_BA"
      Inputs = [ Port Quantum Bob ]
      Outputs = [ Port Quantum Alice ]
      Implementation = Gates.qbit }

let cobit_AB =
    { Name = "cobit_AB"
      Inputs = [ Port Quantum Alice ]
      Outputs = [ Port Quantum Alice; Port Quantum Bob ]
      Implementation = Gates.cobit }

let cobit_BA =
    { Name = "cobit_BA"
      Inputs = [ Port Quantum Bob ]
      Outputs = [ Port Quantum Bob; Port Quantum Alice ]
      Implementation = Gates.cobit }

let cbit_AB =
    { Name = "cbit_AB"
      Inputs = [ Port Quantum Alice ]
      Outputs = [ Port Quantum Bob ]
      Implementation = Gates.cbit }

let cbit_BA =
    { Name = "cbit_BA"
      Inputs = [ Port Quantum Bob ]
      Outputs = [ Port Quantum Alice ]
      Implementation = Gates.cbit }

let CNOT =
    { Name = "CNOT"
      Inputs = [ Port Quantum Any; Port Quantum Any ]
      Outputs = [ Port Quantum Any; Port Quantum Any ]
      Implementation = Gates.CNOT }

let CNOT_AB =
    { Name = "CNOT_AB"
      Inputs = [ Port Quantum Alice; Port Quantum Bob ]
      Outputs = [ Port Quantum Alice; Port Quantum Bob ]
      Implementation = Gates.CNOT }

let CNOT_BA =
    { Name = "CNOT_BA"
      Inputs = [ Port Quantum Bob; Port Quantum Alice ]
      Outputs = [ Port Quantum Bob; Port Quantum Alice ]
      Implementation = Gates.CNOT }

let SWAP =
    { Name = "SWAP"
      Inputs = [ Port Quantum Alice; Port Quantum Bob ]
      Outputs = [ Port Quantum Alice; Port Quantum Bob ]
      Implementation = Gates.SWAP }
