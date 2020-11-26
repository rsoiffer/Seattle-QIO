module internal App

open Board
open Browser.Types
open Circuit
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Gates
open Levels
open Quantum
open ReactArcher
open ReactDraggable
open System

type private Message =
    | AddNode
    | MoveNode of NodeId * Board.Position
    | StartWire of WireCreationState

let private tryMax xs =
    if Seq.isEmpty xs then None else Seq.max xs |> Some

let private change key f map =
    match Map.tryFind key map |> f with
    | Some value -> Map.add key value map
    | None -> map

let private initialBoard =
    { StartNodeId = NodeId 0
      EndNodeId = NodeId 3
      Nodes =
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
      Wires =
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
      WireCreationState = NotDragging }

let private challenge =
    { Free = []
      Costly = []
      Goal = InitCbitRandom }

// let realOutputState, oracleOutputState = testOnce challenge board
// printfn "%s" (prettyPrint realOutputState)
// printfn "%s" (prettyPrint oracleOutputState)

let private printNodePortId (NodeId nodeId) isOutput port =
    sprintf "Node%iType%bPort%i" nodeId isOutput port

let private wireRelation targetId =
    { targetId = targetId
      targetAnchor = Left
      sourceAnchor = Right
      label = None
      style =
          { ArcherStyle.defaults with
                strokeColor = Some "blue"
                strokeWidth = Some 1 }
          |> Some }

let private viewPort dispatch (board: Board) nodeId isOutputPort portId =
    let node = board.Nodes.[nodeId]

    let port =
        if isOutputPort then node.Definition.Outputs.[portId] else node.Definition.Inputs.[portId]

    let classes =
        seq {
            yield "port"
            if port.DataType = Classical then yield "port-classical"
            if port.Party = Alice then yield "port-alice"
            if port.Party = Bob then yield "port-bob"
        }

    let relations =
        if isOutputPort then
            outputWireIds (toCircuit board) nodeId
            |> List.map (fun wireId -> board.Wires.[wireId])
            |> List.filter (fun wire -> wire.Placement.Left.Port = portId)
            |> List.map (fun wire ->
                printNodePortId wire.Placement.Right.NodeId false portId
                |> wireRelation)
            |> List.toArray
        else
            [||]

    archerElement [ Id(printNodePortId nodeId isOutputPort portId)
                    Relations relations ] [
        div [ Class(String.Join(" ", classes))
              OnMouseDown(fun event ->
                  event.preventDefault ()
                  let position = { X = event.clientX; Y = event.clientY }

                  if isOutputPort
                  then FloatingRight({ NodeId = nodeId; Port = portId }, position)
                  else FloatingLeft({ NodeId = nodeId; Port = portId }, position)
                  |> StartWire
                  |> dispatch) ] []
    ]

let private viewNode dispatch (board: Board) (containerRef: IArcherContainer option ref) nodeId =
    let node = board.Nodes.[nodeId]

    draggable [ Cancel ".port"
                OnDrag(fun _ data ->
                    MoveNode(nodeId, { X = data.x; Y = data.y })
                    |> dispatch

                    !containerRef
                    |> Option.iter (fun container -> container.refreshScreen ())

                    true)
                Position(node.Position |> Position.ofBoard) ] [
        div [ Class "node" ] [
            div
                [ Class "portstack" ]
                (node.Definition.Inputs
                 |> idx
                 |> Seq.map (viewPort dispatch board nodeId false))
            div [ Class "nodetitle" ] [
                str node.Definition.Name
            ]
            div
                [ Class "portstack" ]
                (node.Definition.Outputs
                 |> idx
                 |> Seq.map (viewPort dispatch board nodeId true))
        ]
    ]

let private updateFloatingWire state (event: MouseEvent) =
    let position = { X = event.clientX; Y = event.clientY }

    match state with
    | FloatingLeft (inputId, _) -> FloatingLeft(inputId, position)
    | FloatingRight (outputId, _) -> FloatingRight(outputId, position)
    | _ -> state

let private view (board: Board) dispatch =
    let containerRef: IArcherContainer option ref = ref None

    let nodes =
        board.Nodes
        |> Map.toSeq
        |> Seq.map (fun (nodeId, _) -> viewNode dispatch board containerRef nodeId)
        |> div []

    let floatingWire =
        match board.WireCreationState with
        | FloatingLeft (inputId, position) ->
            draggable [ Disabled true
                        Position(Position.ofBoard position) ] [
                div [ Class "floating-wire" ] [
                    archerElement [ Id "floating-wire"
                                    Relations [| printNodePortId inputId.NodeId false inputId.Port
                                                 |> wireRelation |] ] [
                        div [] []
                    ]
                ]
            ]
        | _ -> str ""

    div [ Class "app"
          OnMouseMove
              (updateFloatingWire board.WireCreationState
               >> StartWire
               >> dispatch)
          OnMouseUp(fun _ -> StartWire NotDragging |> dispatch) ] [
        div [ Class "toolbar" ] [
            button [ OnClick <| fun _ -> dispatch AddNode ] [
                str "Add Node"
            ]
        ]
        archerContainer [ Class "board"
                          Ref(fun container ->
                                  if isNull container |> not then
                                      containerRef
                                      := container :?> IArcherContainer |> Some) ] [
            nodes
            floatingWire
        ]
    ]

let private update message (board: Board) =
    match message with
    | AddNode ->
        let nodeId =
            Map.toSeq board.Nodes
            |> Seq.map (fun ((NodeId nodeId), _) -> nodeId)
            |> tryMax
            |> Option.defaultValue 0
            |> (+) 1
            |> NodeId

        let x =
            { Definition = X
              Visibility = NodeVisibility.Normal
              Position = { X = 0.0; Y = 200.0 } }

        { board with
              Nodes = board.Nodes |> Map.add nodeId x }
    | MoveNode (nodeId, position) ->
        { board with
              Nodes =
                  board.Nodes
                  |> change nodeId (Option.map (fun node -> { node with Position = position })) }
    | StartWire creation ->
        printfn "%A" creation

        { board with
              WireCreationState = creation }

Program.mkSimple (fun () -> initialBoard) update view
|> Program.withReactSynchronous "app"
|> Program.run
