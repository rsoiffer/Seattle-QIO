module internal SeattleQIO.App

open Browser.Dom
open Browser.Types
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open System

open SeattleQIO.Board
open SeattleQIO.Circuit
open SeattleQIO.Collections
open SeattleQIO.Gates
open SeattleQIO.Levels
open SeattleQIO.Quantum
open SeattleQIO.ReactArcher
open SeattleQIO.ReactDraggable

type private Message =
    | AddNode
    | MoveNode of NodeId * Board.Position
    | StartWire of WireCreationState
    | EndWire of NodeIOId

let private relativeTo selector position =
    let element =
        document.querySelector selector :?> HTMLElement

    { X = position.X - element.offsetLeft
      Y = position.Y - element.offsetTop }

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
                strokeWidth = Some 2.0
                endShape = Some(upcast {| arrow = {| arrowLength = 0 |} |}) }
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
        [| if isOutputPort then
            yield!
                outputWireIds (toCircuit board) nodeId
                |> Seq.map (fun wireId -> board.Wires.[wireId])
                |> Seq.filter (fun wire -> wire.Placement.Left.Port = portId)
                |> Seq.map (fun wire ->
                    printNodePortId wire.Placement.Right.NodeId false wire.Placement.Right.Port
                    |> wireRelation)

            match board.WireCreationState with
            | FloatingRight (outputId, _) when outputId = { NodeId = nodeId; Port = portId } ->
                yield wireRelation "floating-wire"
            | _ -> () |]

    archerElement [ Id(printNodePortId nodeId isOutputPort portId)
                    Relations relations ] [
        div [ Class(String.Join(" ", classes))
              OnMouseDown(fun event ->
                  event.preventDefault ()

                  let position =
                      { X = event.pageX; Y = event.pageY }
                      |> relativeTo ".board"

                  if isOutputPort
                  then FloatingRight({ NodeId = nodeId; Port = portId }, position)
                  else FloatingLeft({ NodeId = nodeId; Port = portId }, position)
                  |> StartWire
                  |> dispatch)
              OnMouseUp(fun _ ->
                  if isOutputPort
                  then NodeOutputId { NodeId = nodeId; Port = portId }
                  else NodeInputId { NodeId = nodeId; Port = portId }
                  |> EndWire
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
    let position =
        { X = event.pageX; Y = event.pageY }
        |> relativeTo ".board"

    match state with
    | FloatingLeft (inputId, _) -> FloatingLeft(inputId, position)
    | FloatingRight (outputId, _) -> FloatingRight(outputId, position)
    | NotDragging -> NotDragging

let private view (board: Board) dispatch =
    let containerRef: IArcherContainer option ref = ref None

    let nodes =
        board.Nodes
        |> Map.toSeq
        |> Seq.map (fun (nodeId, _) -> viewNode dispatch board containerRef nodeId)
        |> div []

    let floatingWire =
        let relations, position =
            match board.WireCreationState with
            | FloatingLeft (inputId, position) ->
                [| printNodePortId inputId.NodeId false inputId.Port
                   |> wireRelation |],
                position
            | FloatingRight (_, position) -> [||], position
            | NotDragging -> [||], { X = 0.0; Y = 0.0 }

        draggable [ Disabled true
                    Position(Position.ofBoard position) ] [
            div [ Class "floating-wire" ] [
                archerElement [ Id "floating-wire"
                                Relations relations ] [
                    div [] []
                ]
            ]
        ]

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
        board
        |> Board.addNode
            { Definition = X
              Visibility = NodeVisibility.Normal
              Position = { X = 0.0; Y = 200.0 } }
    | MoveNode (nodeId, position) ->
        { board with
              Nodes =
                  board.Nodes
                  |> Map.change nodeId (Option.map (fun node -> { node with Position = position })) }
    | StartWire creation ->
        { board with
              WireCreationState = creation }
    | EndWire nodeId ->
        match nodeId, board.WireCreationState with
        | NodeOutputId outputId, FloatingLeft (inputId, _)
        | NodeInputId inputId, FloatingRight (outputId, _) -> board |> Board.addWire outputId inputId
        | _ -> board

Program.mkSimple (fun () -> initialBoard) update view
|> Program.withReactSynchronous "app"
|> Program.run
