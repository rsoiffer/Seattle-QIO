module App

open Board
open Circuit
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Gates
open Levels
open ReactArcher
open Quantum

type Message = AddNode

let private tryMax xs =
    if Seq.isEmpty xs then None else Seq.max xs |> Some

let inline private draggable props children =
    ofImport "default" "react-draggable" props children

let board =
    { Board.StartNodeId = NodeId 0
      Board.EndNodeId = NodeId 3
      Board.Nodes =
          [ NodeId 0,
            { Definition = startNodeDef []
              Visibility = Normal }
            NodeId 1,
            { Definition = InitQubit
              Visibility = Normal }
            NodeId 2, { Definition = H; Visibility = Normal }
            NodeId 3, { Definition = M; Visibility = Normal }
            NodeId 4,
            { Definition = endNodeDef [ port Classical Any ]
              Visibility = Normal } ]
          |> Map.ofList
      Board.Wires =
          [ WireId 5,
            { Placement =
                  { Left = { NodeId = NodeId 1; Port = 0 }
                    Right = { NodeId = NodeId 2; Port = 0 } }
              Visible = true }
            WireId 6,
            { Placement =
                  { Left = { NodeId = NodeId 2; Port = 0 }
                    Right = { NodeId = NodeId 3; Port = 0 } }
              Visible = true }
            WireId 7,
            { Placement =
                  { Left = { NodeId = NodeId 3; Port = 0 }
                    Right = { NodeId = NodeId 4; Port = 0 } }
              Visible = true } ]
          |> Map.ofList }

let challenge =
    { Free = []
      Costly = []
      Goal = InitCbitRandom }

let realOutputState, oracleOutputState = testOnce challenge board
printfn "%s" (prettyPrint realOutputState)
printfn "%s" (prettyPrint oracleOutputState)

let init () = board

let private view (model: Board) dispatch =
    let addNode =
        button [ OnClick
                 <| fun _ -> dispatch AddNode ] [
            str "Add Node"
        ]

    let printNodeId (NodeId nodeId) = sprintf "Node%i" nodeId
    let printWireId (WireId wireId) = sprintf "Wire%i" wireId

    let makeWire wireId =
        let rightNodeId =
            model.Wires.[wireId].Placement.Right.NodeId

        printfn "Making wire %s to %s" (printWireId wireId) (printNodeId rightNodeId)
        archerElement [ Id(printWireId wireId)
                        Relations [| TargetId(printNodeId rightNodeId)
                                     TargetAnchor Left
                                     SourceAnchor Right
                                     Style [
                                       StrokeColor "blue"; StrokeWidth 1
                                     ] |] ] [
            div [] [ str "Arrow" ]
        ]

    let makeNode nodeId =
        draggable [ Id(printNodeId nodeId) ] [
            div [] [
                div [ Class "box" ] [
                    str model.Nodes.[nodeId].Definition.Name
                ]
                div
                    [ Class "wires" ]
                    (outputWireIds (toCircuit model) nodeId
                     |> List.map makeWire)
            ]
        ]

    let nodes =
        model.Nodes
        |> Map.toSeq
        |> Seq.map (fun (nodeId, _) -> makeNode nodeId)
        |> div []

    div [] [ addNode; archerContainer [] [nodes] ]

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
