module internal SeattleQio.Editor.Program

open Browser.Dom
open Browser.Types
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open System

open SeattleQio.Editor
open SeattleQio.Editor.Board
open SeattleQio.Editor.Collections
open SeattleQio.Editor.Levels
open SeattleQio.Editor.React
open SeattleQio.Editor.React.Archer
open SeattleQio.Editor.React.Draggable
open SeattleQio.Simulator.Circuit
open SeattleQio.Simulator.Gates
open SeattleQio.Simulator.Quantum

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
    { Free = [ H; M ]
      Costly = [ CNOT_AB, 1 ]
      Goal = InitCbitRandom }

let private initialLevel =
    { Challenge = challenge
      Board = initialBoard }

// let realOutputState, oracleOutputState = testOnce challenge board
// printfn "%s" (prettyPrint realOutputState)
// printfn "%s" (prettyPrint oracleOutputState)

let private printNodePortId (NodeId nodeId) isOutput port =
    sprintf "Node%iType%bPort%i" nodeId isOutput port

let private wireRelation dataType targetId =
    { targetId = targetId
      targetAnchor = Left
      sourceAnchor = Right
      label = None
      style =
          { Archer.Style.defaults with
                strokeColor = Some "blue"
                strokeWidth = Some 2.0
                strokeDasharray = if dataType = Classical then Some "5,5" else None
                endShape = Some(upcast {| arrow = {| arrowLength = 0 |} |}) }
          |> Some }

let private viewPort props port =
    let classes =
        seq {
            yield "port"
            if port.DataType = Classical then yield "port-classical"
            if port.Party = Alice then yield "port-alice"
            if port.Party = Bob then yield "port-bob"
        }

    div (Seq.append [ Class(String.Join(" ", classes)) ] props) []

let private viewDraggablePort dispatch (board: Board) nodeId isOutputPort portId =
    let node = board.Nodes.[nodeId]

    let port =
        if isOutputPort then node.Definition.Outputs.[portId] else node.Definition.Inputs.[portId]

    let relations =
        [| if isOutputPort then
            yield!
                outputWireIds (toCircuit board) nodeId
                |> Seq.map (fun wireId -> board.Wires.[wireId])
                |> Seq.filter (fun wire -> wire.Placement.Left.Port = portId)
                |> Seq.map (fun wire ->
                    printNodePortId wire.Placement.Right.NodeId false wire.Placement.Right.Port
                    |> wireRelation
                        (Board.port wire.Placement.Right.NodeId wire.Placement.Right.Port true board)
                            .DataType)

            match board.WireCreationState with
            | FloatingRight (outputId, _) when outputId = { NodeId = nodeId; Port = portId } ->
                yield wireRelation (Board.port nodeId portId true board).DataType "floating-wire"
            | _ -> () |]

    Archer.element [ Id(printNodePortId nodeId isOutputPort portId)
                     Relations relations ] [
        port
        |> viewPort [ OnMouseDown(fun event ->
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
                          |> dispatch) ]
    ]

let private viewNodeDefinition viewPort (node: NodeDefinition) =
    div [ Class "node" ] [
        div [ Class "portstack" ] (node.Inputs |> idx |> Seq.map (viewPort false))
        div [ Class "nodetitle" ] [
            str node.Name
        ]
        div [ Class "portstack" ] (node.Outputs |> idx |> Seq.map (viewPort true))
    ]

let private viewNode dispatch (board: Board) (containerRef: IContainer option ref) nodeId =
    let node = board.Nodes.[nodeId]

    draggable [ Cancel ".port"
                OnDrag(fun _ data ->
                    MoveNode(nodeId, { X = data.x; Y = data.y })
                    |> dispatch

                    !containerRef
                    |> Option.iter (fun container -> container.refreshScreen ())

                    true)
                Position(Position.toDraggable node.Position) ] [
        node.Definition
        |> viewNodeDefinition (viewDraggablePort dispatch board nodeId)
    ]

let private viewFloatingWire board =
    let relations, position =
        match board.WireCreationState with
        | FloatingLeft (inputId, position) ->
            [| printNodePortId inputId.NodeId false inputId.Port
               |> wireRelation
                   (Board.port inputId.NodeId inputId.Port false board)
                       .DataType |],
            position
        | FloatingRight (_, position) -> [||], position
        | NotDragging -> [||], { X = 0.0; Y = 0.0 }

    draggable [ Disabled true
                Position(Position.toDraggable position) ] [
        div [ Class "floating-wire" ] [
            Archer.element [ Id "floating-wire"
                             Relations relations ] [
                div [] []
            ]
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

let private viewPaletteNode node =
    node
    |> viewNodeDefinition (fun isOutput portId ->
        if isOutput then node.Outputs.[portId] else node.Inputs.[portId]
        |> viewPort [])

let private viewPalette dispatch challenge =
    challenge.Free
    @ (challenge.Costly |> List.map fst)
    |> List.map viewPaletteNode
    |> div [ Class "palette" ]

let private view level dispatch =
    let containerRef: IContainer option ref = ref None

    let nodes =
        level.Board.Nodes
        |> Map.toSeq
        |> Seq.map (fun (nodeId, _) -> viewNode dispatch level.Board containerRef nodeId)
        |> div []

    div [ Class "app"
          OnMouseMove
              (updateFloatingWire level.Board.WireCreationState
               >> StartWire
               >> dispatch)
          OnMouseUp(fun _ -> StartWire NotDragging |> dispatch) ] [
        viewPalette dispatch level.Challenge
        Archer.container [ Class "board"
                           Ref(fun container ->
                                   if isNull container |> not
                                   then containerRef := container :?> IContainer |> Some) ] [
            nodes
            viewFloatingWire level.Board
        ]
    ]

let private update message level =
    match message with
    | AddNode ->
        let board' =
            level.Board
            |> Board.addNode
                { Definition = X
                  Visibility = NodeVisibility.Normal
                  Position = { X = 0.0; Y = 200.0 } }

        { level with Board = board' }
    | MoveNode (nodeId, position) ->
        let board' =
            { level.Board with
                  Nodes =
                      level.Board.Nodes
                      |> Map.change nodeId (Option.map (fun node -> { node with Position = position })) }

        { level with Board = board' }
    | StartWire creation ->
        let board' =
            { level.Board with
                  WireCreationState = creation }

        { level with Board = board' }
    | EndWire nodeId ->
        match nodeId, level.Board.WireCreationState with
        | NodeOutputId outputId, FloatingLeft (inputId, _)
        | NodeInputId inputId, FloatingRight (outputId, _) ->
            { level with
                  Board = level.Board |> Board.addWire outputId inputId }
        | _ -> level

Program.mkSimple (fun () -> initialLevel) update view
|> Program.withReactSynchronous "app"
|> Program.run
