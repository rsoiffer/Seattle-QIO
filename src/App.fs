module App

open BoardState
open Browser.Dom
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Quantum

let boardState =
    { Nodes =
          Map.ofSeq [ NodeId 1, GateImplementations.InitQubit
                      NodeId 2, GateImplementations.H ]
      Wires =
          Map.ofSeq [ WireId 3,
                      { Left = { NodeId = NodeId 1; Port = 0 }
                        Right = Some { NodeId = NodeId 2; Port = 0 } }
                      WireId 4,
                      { Left = { NodeId = NodeId 2; Port = 0 }
                        Right = None } ] }

let evalState = eval boardState

// Mutable variable to count the number of times we clicked the button
let mutable count = 0

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton =
    document.querySelector (".my-button") :?> Browser.Types.HTMLButtonElement

// Register our listener
myButton.onclick <-
    fun _ ->
        count <- count + 1
        myButton.innerText <- evalState.ToString()

let inline draggable props children = ofImport "default" "react-draggable" props children
 
let view _ _ = draggable [] [ div [ Class "box" ] [ str "goodbye world" ] ]

Program.mkSimple (fun _ -> ()) (fun _ _ -> ()) view
|> Program.withReactSynchronous "app"
|> Program.run
