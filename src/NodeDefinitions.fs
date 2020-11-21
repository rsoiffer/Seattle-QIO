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

type Node =
    { Name: string
      Inputs: Port list
      Outputs: Port list
      Implementation: GateImplementations.GateImplementation }


let X =
    { Name = "X"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Implementation = GateImplementations.X }

let Z =
    { Name = "Z"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Implementation = GateImplementations.Z }

let H =
    { Name = "H"
      Inputs = [ port Quantum Any ]
      Outputs = [ port Quantum Any ]
      Implementation = GateImplementations.H }

let qbit_AB =
    { Name = "qbit_AB"
      Inputs = [ port Quantum Alice ]
      Outputs = [ port Quantum Bob ]
      Implementation = GateImplementations.qbit }

let qbit_BA =
    { Name = "qbit_BA"
      Inputs = [ port Quantum Bob ]
      Outputs = [ port Quantum Alice ]
      Implementation = GateImplementations.qbit }

let cobit_AB =
    { Name = "cobit_AB"
      Inputs = [ port Quantum Alice ]
      Outputs = [ port Quantum Alice; port Quantum Bob ]
      Implementation = GateImplementations.cobit }

let cobit_BA =
    { Name = "cobit_BA"
      Inputs = [ port Quantum Bob ]
      Outputs = [ port Quantum Bob; port Quantum Alice ]
      Implementation = GateImplementations.cobit }

let cbit_AB =
    { Name = "cbit_AB"
      Inputs = [ port Quantum Alice ]
      Outputs = [ port Quantum Bob ]
      Implementation = GateImplementations.cbit }

let cbit_BA =
    { Name = "cbit_BA"
      Inputs = [ port Quantum Bob ]
      Outputs = [ port Quantum Alice ]
      Implementation = GateImplementations.cbit }

let CNOT =
    { Name = "CNOT"
      Inputs = [ port Quantum Any; port Quantum Any ]
      Outputs = [ port Quantum Any; port Quantum Any ]
      Implementation = GateImplementations.CNOT }

let CNOT_AB =
    { Name = "CNOT_AB"
      Inputs = [ port Quantum Alice; port Quantum Bob ]
      Outputs = [ port Quantum Alice; port Quantum Bob ]
      Implementation = GateImplementations.CNOT }

let CNOT_BA =
    { Name = "CNOT_BA"
      Inputs = [ port Quantum Bob; port Quantum Alice ]
      Outputs = [ port Quantum Bob; port Quantum Alice ]
      Implementation = GateImplementations.CNOT }

let SWAP =
    { Name = "SWAP"
      Inputs = [ port Quantum Alice; port Quantum Bob ]
      Outputs = [ port Quantum Alice; port Quantum Bob ]
      Implementation = GateImplementations.SWAP }
