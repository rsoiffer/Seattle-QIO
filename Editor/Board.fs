namespace SeattleQio.Editor.Board

open SeattleQio.Editor.Collections
open SeattleQio.Editor.React.Draggable
open SeattleQio.Simulator.Circuit
open SeattleQio.Simulator.Gates
open SeattleQio.Simulator.Quantum

type internal Position = { X: float; Y: float }

module internal Position =
    let toDraggable { X = x; Y = y } = { x = x; y = y }


type InferredType =
    { InferredDataType: DataType option
      InferredParty: Party option }

module InferredType =
    let ofPort port =
        { InferredDataType = Some port.DataType
          InferredParty = Some port.Party }


type internal Node =
    { Definition: NodeDefinition
      InferredInputTypes: InferredType list option
      InferredOutputTypes: InferredType list option
      Position: Position }

type internal Wire =
    { Placement: WirePlacement
      InferredType: InferredType option }

type internal WireCreationState =
    | NotDragging
    | FloatingRight of NodeOutputId * Position
    | FloatingLeft of NodeInputId * Position

type internal Board =
    { StartNodeIds: NodeId list
      EndNodeIds: NodeId list
      Nodes: Map<NodeId, Node>
      Wires: Map<WireId, Wire>
      WireCreationState: WireCreationState }

module internal Board =
    let myRandom = System.Random()

    let addNode node (board: Board) =
        let nodeId = NodeId(myRandom.Next())

        { board with
              Nodes = board.Nodes |> Map.add nodeId node }

    let removeNode nodeId board: Board =
        let wires =
            board.Wires
            |> Map.filter (fun _ wire ->
                wire.Placement.Left.NodeId <> nodeId
                && wire.Placement.Right.NodeId <> nodeId)

        { board with
              Nodes = board.Nodes |> Map.remove nodeId
              Wires = wires }

    let randomizeNodeIds board: Board =
        let newNodeIds =
            board.Nodes
            |> Map.map (fun k v -> NodeId(myRandom.Next()))

        let getNewId i = newNodeIds.[i]

        let newNodes =
            board.Nodes
            |> Map.toSeq
            |> Seq.map (fun (a, b) -> (getNewId a, b))
            |> Map.ofSeq

        let newWires =
            board.Wires
            |> Map.map (fun k wire ->
                let newLeft =
                    { wire.Placement.Left with
                          NodeId = getNewId wire.Placement.Left.NodeId }

                let newRight =
                    { wire.Placement.Right with
                          NodeId = getNewId wire.Placement.Right.NodeId }

                { wire with
                      Placement = { Left = newLeft; Right = newRight } })

        { StartNodeIds = board.StartNodeIds |> List.map getNewId
          EndNodeIds = board.EndNodeIds |> List.map getNewId
          Nodes = newNodes
          Wires = newWires
          WireCreationState =
              match board.WireCreationState with
              | NotDragging -> NotDragging
              | FloatingRight (nodeOutputId, pos) ->
                  FloatingRight
                      ({ nodeOutputId with
                             NodeId = getNewId nodeOutputId.NodeId },
                       pos)
              | FloatingLeft (nodeInputId, pos) ->
                  FloatingLeft
                      ({ nodeInputId with
                             NodeId = getNewId nodeInputId.NodeId },
                       pos) }

    let port nodeIoId board =
        match nodeIoId with
        | NodeInputId nodeInputId ->
            (info board.Nodes.[nodeInputId.NodeId].Definition)
                .Inputs.[nodeInputId.InputPort]
        | NodeOutputId nodeInputId ->
            (info board.Nodes.[nodeInputId.NodeId].Definition)
                .Outputs.[nodeInputId.OutputPort]

    let count definition board =
        board.Nodes
        |> Map.filter (fun _ node -> node.Definition = definition)
        |> Map.count

    let addWire (left: NodeOutputId) (right: NodeInputId) (board: Board) =
        let wireId = WireId(myRandom.Next())

        let leftPort = port (NodeOutputId left) board
        let rightPort = port (NodeInputId right) board

        let wire =
            { Placement = { Left = left; Right = right }
              InferredType =
                  Some
                      { InferredDataType =
                            if leftPort.DataType = rightPort.DataType then Some leftPort.DataType else None
                        InferredParty = if leftPort.Party = rightPort.Party then Some leftPort.Party else None } }

        { board with
              Wires =
                  board.Wires
                  |> Map.filter (fun _ wire ->
                      wire.Placement.Left <> left
                      && wire.Placement.Right <> right)
                  |> Map.add wireId wire }

    let canAddWire (left: NodeOutputId) (right: NodeInputId) (board: Board) =
        let leftPort = port (NodeOutputId left) board
        let rightPort = port (NodeInputId right) board

        leftPort.DataType = rightPort.DataType
        && (leftPort.Party = Any
            || rightPort.Party = Any
            || leftPort.Party = rightPort.Party)

    let inferTypes board =
        let allNodeIOIds nodeId =
            let info = info board.Nodes.[nodeId].Definition

            (info.Inputs
             |> List.mapi (fun portId port -> NodeInputId { NodeId = nodeId; InputPort = portId }, port))
            @ (info.Outputs
               |> List.mapi (fun portId port -> NodeOutputId { NodeId = nodeId; OutputPort = portId }, port))

        let reachableNodeIOIds nodeIOId =
            if (port nodeIOId board).Party = Any then
                NodeIOId.nodeId nodeIOId
                |> allNodeIOIds
                |> Seq.filter (fun (_, port) -> port.Party = Any)
                |> Seq.map fst
                |> Seq.filter ((<>) nodeIOId)
            else
                Seq.empty

        let connectedWires nodeIOId =
            board.Wires
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun wire ->
                match nodeIOId with
                | NodeInputId input -> input = wire.Placement.Right
                | NodeOutputId output -> output = wire.Placement.Left)

        let neighbors wire =
            [ NodeOutputId wire.Placement.Left
              NodeInputId wire.Placement.Right ]
            |> Seq.collect reachableNodeIOIds
            |> Seq.collect connectedWires
            |> Set.ofSeq

        let rec ``find the components and stuff`` (visited: Wire Set) (wire: Wire): Wire Set =
            Set.difference (neighbors wire) visited
            |> Set.fold (fun component' neighbor ->
                ``find the components and stuff`` (Set.union visited component') neighbor
                |> Set.union component') (Set.singleton wire)

        { board with
              Wires =
                  board.Wires
                  |> Map.map (fun _ wire ->
                      let component' =
                          ``find the components and stuff`` Set.empty wire

                      let hasAlice =
                          component'
                          |> Set.exists (fun wire ->
                              (port (NodeOutputId wire.Placement.Left) board)
                                  .Party = Alice
                              || (port (NodeInputId wire.Placement.Right) board)
                                  .Party = Alice)

                      let hasBob =
                          component'
                          |> Set.exists (fun wire ->
                              (port (NodeOutputId wire.Placement.Left) board)
                                  .Party = Bob
                              || (port (NodeInputId wire.Placement.Right) board)
                                  .Party = Bob)

                      { wire with
                            InferredType =
                                match hasAlice, hasBob with
                                | true, false ->
                                    Some
                                        { InferredDataType =
                                              wire.InferredType
                                              |> Option.bind (fun inferred -> inferred.InferredDataType)
                                          InferredParty = Some Alice }
                                | false, true ->
                                    Some
                                        { InferredDataType =
                                              wire.InferredType
                                              |> Option.bind (fun inferred -> inferred.InferredDataType)
                                          InferredParty = Some Bob }
                                | false, false ->
                                    Some
                                        { InferredDataType =
                                              wire.InferredType
                                              |> Option.bind (fun inferred -> inferred.InferredDataType)
                                          InferredParty = Some Any }
                                | true, true ->
                                    Some
                                        { InferredDataType =
                                              wire.InferredType
                                              |> Option.bind (fun inferred -> inferred.InferredDataType)
                                          InferredParty = None } }) }
