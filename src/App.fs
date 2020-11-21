module App

open Circuit
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Gates
open Quantum

let inline draggable props children =
    ofImport "default" "react-draggable" props children

let boardState =
    { Nodes =
          Map.ofSeq [ NodeId 1, gate_InitQubit
                      NodeId 2, gate_H
                      NodeId 3, gate_M
                      NodeId 4, gate_DestroyCbit ]
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
    [ NodeId 1, InitQubit; NodeId 2, H ] |> Map.ofSeq

let view model _ =
    model
    |> Map.toSeq
    |> Seq.map (fun (_, node) ->
        draggable [] [
            div [ Class "box" ] [ str node.Name ]
        ])
    |> div []

Program.mkSimple init (fun _ -> id) view
|> Program.withReactSynchronous "app"
|> Program.run
