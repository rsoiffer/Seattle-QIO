module App

open Board
open Circuit
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Gates
open Quantum

type Message = AddNode

let private tryMax xs =
    if Seq.isEmpty xs then None else Seq.max xs |> Some

let inline private draggable props children =
    ofImport "default" "react-draggable" props children

let private boardState =
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

let private init () =
    let initQubit =
        { Definition = InitQubit
          Visibility = NodeVisibility.Normal }

    let h =
        { Definition = H
          Visibility = NodeVisibility.Normal }

    { Board.Nodes = [ NodeId 1, initQubit; NodeId 2, h ] |> Map.ofList
      Board.Wires = Map.empty }

let private view (model: Board) dispatch =
    let addNode =
        button [ OnClick <| fun _ -> dispatch AddNode ] [
            str "Add Node"
        ]

    let nodes =
        model.Nodes
        |> Map.toSeq
        |> Seq.map (fun (_, node) ->
            draggable [] [
                div [ Class "box" ] [
                    str node.Definition.Name
                ]
            ])
        |> div []

    div [] [ addNode; nodes ]

let private update message (model: Board) =
    match message with
    | AddNode ->
        let nodeId =
            Map.toSeq model.Nodes
            |> Seq.map (fun ((NodeId nodeId), _) -> nodeId)
            |> tryMax
            |> Option.defaultValue 0
            |> (+) 1
            |> NodeId

        let x =
            { Definition = X
              Visibility = NodeVisibility.Normal }

        { model with
              Nodes = model.Nodes |> Map.add nodeId x }

Program.mkSimple init update view
|> Program.withReactSynchronous "app"
|> Program.run
