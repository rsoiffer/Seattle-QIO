module App

open Board
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
          Map.ofSeq [ NodeId 1, gate_InitBit
                      NodeId 2, gate_H
                      NodeId 3, gate_M
                      NodeId 4, gate_DestroyBit ]
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
    let initQubit =
        { Definition = InitQubit
          Visibility = Normal }

    let h = { Definition = H; Visibility = Normal }

    { Board.StartNodeId = NodeId 1
      Board.EndNodeId = NodeId 2
      Board.Nodes = [ NodeId 1, initQubit; NodeId 2, h ] |> Map.ofList
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
