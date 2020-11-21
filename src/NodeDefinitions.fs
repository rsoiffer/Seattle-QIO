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

let port d p = { DataType = d; Party = p }

type NodeDefinition =
    { Name: string
      Inputs: Port list
      Outputs: Port list
      Implementation: Gates.Gate }


let X =
    { Name = "X"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Implementation = Gates.X }

let Z =
    { Name = "Z"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Implementation = Gates.Z }

let H =
    { Name = "H"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Implementation = Gates.H }

let qbit_AB =
    { Name = "qbit_AB"
      Inputs = [ port Quantum Alice ]
      Outputs = [ port Quantum Bob ]
      Implementation = Gates.qbit }

let qbit_BA =
    { Name = "qbit_BA"
      Inputs = [ port Quantum Bob ]
      Outputs = [ port Quantum Alice ]
      Implementation = Gates.qbit }

let cobit_AB =
    { Name = "cobit_AB"
      Inputs = [ port Quantum Alice ]
      Outputs = [ port Quantum Alice; port Quantum Bob ]
      Implementation = Gates.cobit }

let cobit_BA =
    { Name = "cobit_BA"
      Inputs = [ port Quantum Bob ]
      Outputs = [ port Quantum Bob; port Quantum Alice ]
      Implementation = Gates.cobit }

let cbit_AB =
    { Name = "cbit_AB"
      Inputs = [ port Quantum Alice ]
      Outputs = [ port Quantum Bob ]
      Implementation = Gates.cbit }

let cbit_BA =
    { Name = "cbit_BA"
      Inputs = [ port Quantum Bob ]
      Outputs = [ port Quantum Alice ]
      Implementation = Gates.cbit }

let CNOT =
    { Name = "CNOT"
      Inputs = [ port Quantum Any; port Quantum Any ]
      Outputs = [ port Quantum Any; port Quantum Any ]
      Implementation = Gates.CNOT }

let CNOT_AB =
    { Name = "CNOT_AB"
      Inputs = [ port Quantum Alice; port Quantum Bob ]
      Outputs = [ port Quantum Alice; port Quantum Bob ]
      Implementation = Gates.CNOT }

let CNOT_BA =
    { Name = "CNOT_BA"
      Inputs = [ port Quantum Bob; port Quantum Alice ]
      Outputs = [ port Quantum Bob; port Quantum Alice ]
      Implementation = Gates.CNOT }

let SWAP =
    { Name = "SWAP"
      Inputs = [ port Quantum Alice; port Quantum Bob ]
      Outputs = [ port Quantum Alice; port Quantum Bob ]
      Implementation = Gates.SWAP }