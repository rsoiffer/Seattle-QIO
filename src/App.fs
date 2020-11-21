module App

open BoardState
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open NodeDefinitions
open Quantum

let inline draggable props children = ofImport "default" "react-draggable" props children

let boardState =
    { Nodes =
          Map.ofSeq [ NodeId 1, GateImplementations.InitQubit
                      NodeId 2, GateImplementations.H ]
      Wires =
          Map.ofSeq [ WireId 3,
                      { Left = { NodeId = NodeId 1; Port = 0 }
                        Right = Some { NodeId = NodeId 2; Port = 0 } }
                      WireId 4,
                      { Left = { NodeId = NodeId 2; Port = 0 }
                        Right = None } ] }

eval boardState |> printfn "%A"

let init () =
    let initQubit =
        { Name = "InitQubit"
          Inputs = []
          Outputs = []
          Implementation = GateImplementations.InitQubit }
    let h =
        { Name = "H"
          Inputs = []
          Outputs = []
          Implementation = GateImplementations.H }
    [ NodeId 1, initQubit
      NodeId 2, h ]
    |> Map.ofSeq

let view model _ =
    model
    |> Map.toSeq
    |> Seq.map (fun (_, node) -> draggable [] [ div [ Class "box" ] [ str node.Name ] ])
    |> div []

Program.mkSimple init (fun _ -> id) view
|> Program.withReactSynchronous "app"
|> Program.run
