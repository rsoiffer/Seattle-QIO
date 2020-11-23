module internal ReactDraggable

open Board
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Fable.React

type DraggableData =
    { node: HTMLElement
      x: float
      y: float
      deltaX: float
      deltaY: float
      lastX: float
      lastY: float }

type DraggableEventHandler = delegate of Event * DraggableData -> bool

type Position = { x: float; y: float }

module Position =
    let ofBoard { X = x; Y = y } = { x = x; y = y }

type DraggableProp =
    | Position of Position
    | OnStop of DraggableEventHandler
    | Id of string

let inline draggable props children =
    ofImport "default" "react-draggable" (keyValueList CaseRules.LowerFirst props) children
