module App

open Board
open Circuit
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Gates
open Levels
open Quantum

let inline draggable props children =
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
            { Definition = endNodeDef [ port Quantum Any ]
              Visibility = Normal } ]
          |> Map.ofList
      Board.Wires =
          [ WireId 0,
            { Placement =
                  { Left = { NodeId = NodeId 1; Port = 0 }
                    Right = { NodeId = NodeId 2; Port = 0 } }
              Visible = true }
            WireId 1,
            { Placement =
                  { Left = { NodeId = NodeId 2; Port = 0 }
                    Right = { NodeId = NodeId 3; Port = 0 } }
              Visible = true }
            WireId 2,
            { Placement =
                  { Left = { NodeId = NodeId 3; Port = 0 }
                    Right = { NodeId = NodeId 4; Port = 0 } }
              Visible = true } ]
          |> Map.ofList }

eval (toCircuit board) |> printfn "%A"

let init () = board

let view (model: Board) _ =
    model.Nodes
    |> Map.toSeq
    |> Seq.map (fun (_, node) ->
        draggable [] [
            div [ Class "box" ] [
                str node.Definition.Name
            ]
        ])
    |> div []

Program.mkSimple init (fun _ -> id) view
|> Program.withReactSynchronous "app"
|> Program.run
