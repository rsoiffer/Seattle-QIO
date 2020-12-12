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

type private Evaluation =
    { InputState: MixedState
      OutputState: MixedState
      ExpectedOutputState: MixedState
      Result: string }

type private Model =
    { Level: Level
      Evaluation: Evaluation option }

type private Message =
    | LoadLevel of Level
    | AddNode of NodeDefinition * Board.Position
    | MoveNode of NodeId * Board.Position
    | RemoveNode of NodeId
    | StartWire of WireCreationState
    | EndWire of NodeIOId
    | Evaluate

let private init () =
    { Level = emptyLevelFrom challenge_quantumCoinFlip
      Evaluation = None }

let private levels =
    [ "Level 1", emptyLevelFrom challenge_quantumCoinFlip
      "Level 2", emptyLevelFrom challenge_swap
      "Level 3", emptyLevelFrom challenge_qbit_to_ebit
      "A1Q1 Part a", emptyLevelFrom challenge_a1q1_a
      "A1Q1 Part b", emptyLevelFrom challenge_a1q1_b
      "A1Q1 Part c", emptyLevelFrom challenge_a1q1_c
      "A1Q1 Part d", emptyLevelFrom challenge_a1q1_d
      "A1Q1 Part e", emptyLevelFrom challenge_a1q1_e ]

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

let private wireRelation inferredType targetId =
    let color =
        match inferredType with
        | Some it ->
            match it.InferredParty with
            | Some Any -> Some "black"
            | Some Alice -> Some "red"
            | Some Bob -> Some "blue"
            | None -> Some "pink"
        | None -> Some "pink"

    let dasharray =
        match inferredType with
        | Some it ->
            match it.InferredDataType with
            | Some Classical -> Some "5,5"
            | Some Quantum -> None
            | None -> Some "1,5"
        | None -> Some "1,5"

    { targetId = targetId
      targetAnchor = Left
      sourceAnchor = Right
      label = None
      style =
          { Archer.Style.defaults with
                strokeColor = color
                strokeWidth = Some 2.0
                strokeDasharray = dasharray
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
                wireRelation wire.Value.InferredType (printNodePortId nodeIoId))

           match board.WireCreationState with
           | FloatingRight (outputId, _) when (NodeOutputId outputId) = nodeIoId ->
               yield wireRelation (InferredType.ofPort myPort |> Some) "floating-wire"
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
               |> wireRelation
                   (Board.port (NodeInputId inputId) board
                    |> InferredType.ofPort
                    |> Some) |],
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

let private viewMixedState (mixedState: MixedState) =
    let all1 =
        mixedState
        |> SparseVector.toSeq
        |> Seq.map (fst >> fst)
        |> Set.ofSeq

    let all2 =
        mixedState
        |> SparseVector.toSeq
        |> Seq.map (fst >> snd)
        |> Set.ofSeq


    let prettyPrintBits (Bits b) =
        b
        |> Map.toSeq
        |> Seq.map (fun b -> if snd b then "1" else "0")
        |> Seq.fold (+) ""
        |> sprintf "|%s⟩"

    div [ Class "mixedstate" ] [
        table [] [
            // Header row
            yield
                tr [] [
                    yield td [] []
                    for b1 in all1 do
                        yield td [] [ str (prettyPrintBits b1) ]
                ]
            // Body
            for b2 in all2 do
                yield
                    tr [] [
                        yield td [] [ str (prettyPrintBits b2) ]
                        for b1 in all1 do
                            let c = SparseVector.read (b1, b2) mixedState

                            let s =
                                if abs (c.r) < 0.01 && abs (c.i) < 0.01 then "0"
                                else if abs (c.r) >= 0.01 && abs (c.i) < 0.01 then sprintf "%.2f" c.r
                                else if abs (c.r) < 0.01 && abs (c.i) >= 0.01 then sprintf "%.2f i" c.i
                                else sprintf "%.2f + %.2f i" c.r c.i

                            yield td [] [ str s ]
                    ]
        ]
    ]

let private viewEvaluation dispatch evaluation =
    div [ Class "evaluation" ] [
        yield
            button [ OnClick
                     <| fun _ -> dispatch Evaluate ] [
                str "Evaluate"
            ]
        match evaluation with
        | Some eval ->
            yield str eval.Result
            yield viewMixedState eval.InputState
            yield viewMixedState eval.OutputState
            yield viewMixedState eval.ExpectedOutputState
        | None -> ()
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
    | LoadLevel level ->
        { model with
              Level =
                  { level with
                        Board = level.Board |> Board.randomizeNodeIds }
              Evaluation = None }
    | AddNode (node, position) ->
        let board =
            model.Level.Board
            |> Board.addNode
                { Definition = node
                  InferredInputTypes = None
                  InferredOutputTypes = None
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
        if List.contains nodeId model.Level.Board.StartNodeIds
           || List.contains nodeId model.Level.Board.EndNodeIds then
            model
        else
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
            if not (Board.canAddWire outputId inputId model.Level.Board) then
                model
            else
                let board =
                    model.Level.Board
                    |> Board.addWire outputId inputId

                { model with
                      Level = { model.Level with Board = board } }
        | _ -> model
    | Evaluate ->
        let evaluation =
            try
                let inputState, outputState, expectedOutputState = testOnce model.Level

                let result =
                    if outputState
                       |> SparseVector.approximately 1e-3 expectedOutputState then
                        "equal"
                    else
                        "not equal"

                { InputState = inputState
                  OutputState = outputState
                  ExpectedOutputState = expectedOutputState
                  Result = result }
            with ex ->
                { InputState = SparseVector.zero
                  OutputState = SparseVector.zero
                  ExpectedOutputState = SparseVector.zero
                  Result = "Error: " + ex.Message }

        { model with
              Evaluation = Some evaluation }

Program.mkSimple init update view
|> Program.withReactSynchronous "app"
|> Program.run
