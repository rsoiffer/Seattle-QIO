module App

open Board
open Circuit
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open NodeDefinitions
open Quantum

let inline draggable props children =
    ofImport "default" "react-draggable" props children

let boardState =
    { Nodes =
          Map.ofSeq [ NodeId 1, Gates.InitQubit
                      NodeId 2, Gates.H
                      NodeId 3, Gates.M
                      NodeId 4, Gates.DestroyCbit ]
      Wires =
          Map.ofSeq [ WireId 5,
                      { Left = { NodeId = NodeId 1; Port = 0 }
                        Right = { NodeId = NodeId 2; Port = 0 } }
                      WireId 6,
                      { Left = { NodeId = NodeId 2; Port = 0 }
                        Right = { NodeId = NodeId 3; Port = 0 } }
                      WireId 7,
                      { Left = { NodeId = NodeId 3; Port = 0 }
                        Right = { NodeId = NodeId 4; Port = 0 } } ] }

eval boardState |> printfn "%A"

let init () =
    let initQubitDefinition =
        { Name = "InitQubit"
          Inputs = []
          Outputs = []
          Implementation = Gates.InitQubit }

    let initQubit =
        { Definition = initQubitDefinition
          Visibility = NodeVisibility.Normal }

    let hDefinition =
        { Name = "H"
          Inputs = []
          Outputs = []
          Implementation = Gates.H }

    let h =
        { Definition = hDefinition
          Visibility = NodeVisibility.Normal }

    { Board.Nodes = [ NodeId 1, initQubit; NodeId 2, h ] |> Map.ofList
      Board.Wires = Map.empty }

let view (model: Board) _ =
    model.Nodes
    |> Map.toSeq
    |> Seq.map (fun (_, node) ->
        draggable [] [
            div [ Class "box" ] [
                str node.Definition.Name
            ]
        ])
    |> div []

Program.mkSimple init (fun _ -> id) view
|> Program.withReactSynchronous "app"
|> Program.run
