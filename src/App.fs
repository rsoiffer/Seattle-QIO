module internal App

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
              Position = { X = 800.0; Y = 0.0 } }
            NodeId 5,
            { Definition = CNOT_AB
              Visibility = Normal
              Position = { X = 500.0; Y = 100.0 } } ]
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
          |> Map.ofList
      Board.WireCreationState = NotDragging }

let challenge =
    { Free = []
      Costly = []
      Goal = InitCbitRandom }

// let realOutputState, oracleOutputState = testOnce challenge board
// printfn "%s" (prettyPrint realOutputState)
// printfn "%s" (prettyPrint oracleOutputState)

let init () = board

let private view (model: Board) dispatch =
    let addNode =
        button [ OnClick
                 <| fun _ -> dispatch AddNode ] [
            str "Add Node"
        ]

    let printNodePortId (NodeId nodeId) isOutput port =
        sprintf "Node%iType%bPort%i" nodeId isOutput port

    let makeWire portId wireId =
        let rightNodeId =
            model.Wires.[wireId].Placement.Right.NodeId

        { targetId = printNodePortId rightNodeId false portId
          targetAnchor = Left
          sourceAnchor = Right
          label = None
          style =
              { ArcherStyle.defaults with
                    strokeColor = Some "blue"
                    strokeWidth = Some 1 }
              |> Some }

    let makePort nodeId isOutputPort portId =
        let node = model.Nodes.[nodeId]

        let port =
            if isOutputPort then node.Definition.Outputs.[portId] else node.Definition.Inputs.[portId]

        let myClass =
            System.String.Join
                (" ",
                 seq {
                     yield "port"
                     if port.DataType = Classical then yield "port-classical"
                     if port.Party = Alice then yield "port-alice"
                     if port.Party = Bob then yield "port-bob"
                 })

        let relations =
            if isOutputPort then
                outputWireIds (toCircuit model) nodeId
                |> List.filter (fun wireId -> model.Wires.[wireId].Placement.Left.Port = portId)
                |> List.map (makeWire portId)
                |> Array.ofList
            else
                [||]

        archerElement [ Id(printNodePortId nodeId isOutputPort portId)
                        Relations relations ] [
            div [ Class myClass ] []
        ]

    let makeNode nodeId =
        let node = model.Nodes.[nodeId]
        draggable [ Position(node.Position |> Position.ofBoard)
                    OnDrag(fun _ data ->
                        SetNodePosition(nodeId, { X = data.x; Y = data.y })
                        |> dispatch
                        true) ] [
            div [ Class "node" ] [
                div
                    [ Class "portstack" ]
                    (node.Definition.Inputs
                     |> idx
                     |> Seq.map (makePort nodeId false))
                div [ Class "nodetitle" ] [
                    str node.Definition.Name
                ]
                div
                    [ Class "portstack" ]
                    (node.Definition.Outputs
                     |> idx
                     |> Seq.map (makePort nodeId true))
            ]
        ]

    let nodes =
        model.Nodes
        |> Map.toSeq
        |> Seq.map (fun (nodeId, _) -> makeNode nodeId)
        |> div []

    div [ Class "app" ] [
        div [ Class "toolbar" ] [ addNode ]
        archerContainer [ Class "board" ] [
            nodes
        ]
    ]

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
              Position = { X = 0.0; Y = 200.0 } }

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
