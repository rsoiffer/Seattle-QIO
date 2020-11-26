module internal SeattleQio.Editor.React.Draggable

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

type DraggableEventHandler = Event -> DraggableData -> bool

type Position = { x: float; y: float }

type DraggableProp =
    | Cancel of string
    | Disabled of bool
    | OnDrag of DraggableEventHandler
    | Position of Position

let inline draggable props children =
    ofImport "default" "react-draggable" (keyValueList CaseRules.LowerFirst props) children
