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
open SeattleQio.Simulator
open SeattleQio.Simulator.Circuit
open SeattleQio.Simulator.Gates
open SeattleQio.Simulator.Quantum

type private Model = { Level: Level; Evaluation: string }

type private Message =
    | LoadLevel of Level
    | AddNode of NodeDefinition * Board.Position
    | MoveNode of NodeId * Board.Position
    | RemoveNode of NodeId
    | StartWire of WireCreationState
    | EndWire of NodeIOId
    | Evaluate

let private init () =
    { Level = level_quantumCoinFlip
      Evaluation = "" }

let private levels =
    [ "Level 1", level_quantumCoinFlip
      "Level 2", level_swap
      "Level 3", level_qbit_to_ebit ]

// let realOutputState, oracleOutputState = testOnce level_quantumCoinFlip
// printfn "%s" (prettyPrint realOutputState)
// printfn "%s" (prettyPrint oracleOutputState)

let private printNodePortId nodeIoId =
    match nodeIoId with
    | NodeInputId { NodeId = (NodeId nodeId); InputPort = port } -> sprintf "Node%iInputPort%i" nodeId port
    | NodeOutputId { NodeId = (NodeId nodeId); OutputPort = port } -> sprintf "Node%iOutputPort%i" nodeId port

let private relativeTo selector position =
    let element =
        document.querySelector selector :?> HTMLElement

    { X = position.X - element.offsetLeft
      Y = position.Y - element.offsetTop }

let private isWithinBoard position =
    let board =
        document.querySelector ".board" :?> HTMLElement

    position.X > 0.0
    && position.X < board.offsetWidth
    && position.Y > 0.0
    && position.Y < board.offsetHeight

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

let private viewDraggablePort dispatch (board: Board) nodeIoId =
    let myPort = Board.port nodeIoId board

    let relations =
        [| yield!
            board.Wires
            |> Seq.filter (fun wire -> NodeOutputId wire.Value.Placement.Left = nodeIoId)
            |> Seq.map (fun wire ->
                let nodeIoId = (NodeInputId wire.Value.Placement.Right)
                wireRelation myPort.DataType (printNodePortId nodeIoId))

           match board.WireCreationState with
           | FloatingRight (outputId, _) when (NodeOutputId outputId) = nodeIoId ->
               yield wireRelation myPort.DataType "floating-wire"
           | _ -> () |]

    Archer.element [ Id(printNodePortId nodeIoId)
                     Relations relations ] [
        viewPort
            [ OnMouseDown(fun event ->
                event.preventDefault ()

                let position =
                    { X = event.pageX; Y = event.pageY }
                    |> relativeTo ".board"

                let wireCreationState =
                    match nodeIoId with
                    | NodeInputId n -> FloatingLeft(n, position)
                    | NodeOutputId n -> FloatingRight(n, position)

                wireCreationState |> StartWire |> dispatch)
              OnMouseUp(fun _ -> nodeIoId |> EndWire |> dispatch) ]
            myPort
    ]

let private viewNodeDefinition container
                               (viewInputPort: int -> ReactElement)
                               (viewOutputPort: int -> ReactElement)
                               (node: NodeDefinition)
                               =
    div [ Class "node"
          Ref(fun element -> if isNull element then container () |> Container.refreshScreen) ] [
        div [ Class "portstack" ] (node.Inputs |> idx |> Seq.map viewInputPort)
        div [ Class "nodetitle" ] [
            str node.Name
        ]
        div [ Class "portstack" ] (node.Outputs |> idx |> Seq.map viewOutputPort)
    ]

let private viewNode dispatch (board: Board) container nodeId =
    let node = board.Nodes.[nodeId]

    draggable [ Key(string nodeId)
                Cancel ".port"
                OnDrag(fun _ data ->
                    MoveNode(nodeId, { X = data.x; Y = data.y })
                    |> dispatch

                    true)
                OnStop(fun _ data ->
                    if isWithinBoard { X = data.x; Y = data.y } then
                        true
                    else
                        RemoveNode nodeId |> dispatch
                        false)
                Position(Position.toDraggable node.Position) ] [
        node.Definition
        |> viewNodeDefinition container (fun i ->
               viewDraggablePort dispatch board (NodeInputId { NodeId = nodeId; InputPort = i })) (fun i ->
               viewDraggablePort dispatch board (NodeOutputId { NodeId = nodeId; OutputPort = i }))
    ]

let private viewFloatingWire board =
    let relations, position =
        match board.WireCreationState with
        | FloatingLeft (inputId, position) ->
            [| printNodePortId (NodeInputId inputId)
               |> wireRelation (Board.port (NodeInputId inputId) board).DataType |],
            position
        | FloatingRight (_, position) -> [||], position
        | NotDragging -> [||], { X = 500.0; Y = 500.0 }

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

let private viewPaletteNode dispatch (node, available) =
    div [] [
        available
        |> Option.map string
        |> Option.defaultValue "∞"
        |> str

        draggable [ Disabled(available |> Option.exists ((>=) 0))
                    Position { x = 0.0; y = 0.0 }
                    OnStop(fun _ data ->
                        let position =
                            { X = data.node.offsetLeft + data.x
                              Y = data.node.offsetTop + data.y }
                            |> relativeTo ".board"

                        if isWithinBoard position then AddNode(node, position) |> dispatch
                        true) ] [
            viewNodeDefinition
                (fun () -> Container.empty)
                (fun i -> viewPort [] node.Inputs.[i])
                (fun i -> viewPort [] node.Outputs.[i])
                node
        ]
    ]

let private viewPalette dispatch level =
    (level.Challenge.Free
     |> List.map (fun node -> node, None))
    @ (level.Challenge.Costly
       |> List.map (fun (node, budget) -> node, Some(budget - Board.count node level.Board)))
    |> List.map (viewPaletteNode dispatch)
    |> div [ Class "palette" ]

let private viewEvaluation dispatch evaluation =
    div [ Class "evaluation" ] [
        button [ OnClick
                 <| fun _ -> dispatch Evaluate ] [
            str "Evaluate"
        ]
        str evaluation
    ]

let private viewChallenge dispatch model =
    div [ Class "challenge" ] [
        p [] [
            str model.Level.Challenge.Description
        ]
        viewEvaluation dispatch model.Evaluation
    ]

let private viewLevel dispatch model containerRef =
    let nodes =
        model.Level.Board.Nodes
        |> Map.toSeq
        |> Seq.map (fun (nodeId, _) -> viewNode dispatch model.Level.Board (fun () -> !containerRef) nodeId)
        |> div []

    div [ Class "level"
          OnMouseMove(fun event ->
              if model.Level.Board.WireCreationState <> NotDragging then
                  event
                  |> updateFloatingWire model.Level.Board.WireCreationState
                  |> StartWire
                  |> dispatch)
          OnMouseUp(fun _ -> StartWire NotDragging |> dispatch) ] [
        viewChallenge dispatch model
        Archer.container [ Class "board"
                           Ref(fun container -> if isNull container |> not then containerRef := container :?> IContainer) ] [
            nodes
            viewFloatingWire model.Level.Board
        ]
        viewPalette dispatch model.Level
    ]

let private viewLevelSelect dispatch =
    levels
    |> List.map (fun (name, level) ->
        button [ OnClick(fun _ -> LoadLevel level |> dispatch) ] [
            str name
        ])
    |> div [ Class "level-select" ]

let private view model dispatch =
    let containerRef = ref Container.empty

    div [ Class "app" ] [
        viewLevelSelect dispatch
        viewLevel dispatch model containerRef
    ]

let private update message model =
    match message with
    | LoadLevel level -> { model with Level = level }
    | AddNode (node, position) ->
        let board =
            model.Level.Board
            |> Board.addNode
                { Definition = node
                  Visibility = NodeVisibility.Normal
                  Position = position }

        { model with
              Level = { model.Level with Board = board } }
    | MoveNode (nodeId, position) ->
        let board =
            { model.Level.Board with
                  Nodes =
                      model.Level.Board.Nodes
                      |> Map.change nodeId (Option.map (fun node -> { node with Position = position })) }

        { model with
              Level = { model.Level with Board = board } }
    | RemoveNode nodeId ->
        let board =
            Board.removeNode nodeId model.Level.Board
            |> Board.randomizeNodeIds

        { model with
              Level = { model.Level with Board = board } }
    | StartWire creation ->
        let board =
            { model.Level.Board with
                  WireCreationState = creation }

        { model with
              Level = { model.Level with Board = board } }
    | EndWire nodeId ->
        match nodeId, model.Level.Board.WireCreationState with
        | NodeOutputId outputId, FloatingLeft (inputId, _)
        | NodeInputId inputId, FloatingRight (outputId, _) ->
            let board =
                model.Level.Board
                |> Board.addWire outputId inputId

            { model with
                  Level = { model.Level with Board = board } }
        | _ -> model
    | Evaluate ->
        let evaluation =
            try
                let state1, state2 = testOnce model.Level

                if state1 |> SparseVector.approximately 1e-3 state2
                then "equal"
                else "not equal"
            with ex -> ex.Message

        { model with Evaluation = evaluation }

Program.mkSimple init update view
|> Program.withReactSynchronous "app"
|> Program.run
