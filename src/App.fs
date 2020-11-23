module internal App

open Board
open Circuit
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Gates
open Levels
open Quantum
open ReactDraggable

type Message =
    | AddNode
    | SetNodePosition of NodeId * Board.Position

let private tryMax xs =
    if Seq.isEmpty xs then None else Seq.max xs |> Some

let private change key f map =
    match Map.tryFind key map |> f with
    | Some value -> Map.add key value map
    | None -> map

let board =
    { Board.StartNodeId = NodeId 0
      Board.EndNodeId = NodeId 3
      Board.Nodes =
          [ NodeId 0,
            { Definition = startNodeDef []
              Visibility = Normal
              Position = { X = 0.0; Y = 0.0 } }
            NodeId 1,
            { Definition = InitQubit
              Visibility = Normal
              Position = { X = 200.0; Y = 0.0 } }
            NodeId 2,
            { Definition = H
              Visibility = Normal
              Position = { X = 400.0; Y = 0.0 } }
            NodeId 3,
            { Definition = M
              Visibility = Normal
              Position = { X = 600.0; Y = 0.0 } }
            NodeId 4,
            { Definition = endNodeDef [ port Classical Any ]
              Visibility = Normal
              Position = { X = 800.0; Y = 0.0 } } ]
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
        button [ OnClick <| fun _ -> dispatch AddNode ] [
            str "Add Node"
        ]

    let nodes =
        model.Nodes
        |> Map.toSeq
        |> Seq.map (fun (nodeId, node) ->
            draggable [ node.Position |> Position.ofBoard |> Position
                        OnStop(fun _ data ->
                            SetNodePosition(nodeId, { X = data.x; Y = data.y })
                            |> dispatch

                            true) ] [
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
              Visibility = NodeVisibility.Normal
              Position = { X = 0.0; Y = 50.0 } }

        { model with
              Nodes = model.Nodes |> Map.add nodeId x }
    | SetNodePosition (nodeId, position) ->
        { model with
              Nodes =
                  model.Nodes
                  |> change nodeId (Option.map (fun node -> { node with Position = position })) }

Program.mkSimple init update view
|> Program.withReactSynchronous "app"
|> Program.run
