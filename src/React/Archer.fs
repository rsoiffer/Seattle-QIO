namespace SeattleQIO.React.Archer

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props

type internal IContainer =
    abstract refreshScreen: unit -> unit

type internal Style =
    { strokeColor: string option
      strokeWidth: float option
      strokeDashArray: int option
      noCurves: bool option
      endShape: obj option }

module internal Style =
    let defaults =
        { strokeColor = None
          strokeWidth = None
          strokeDashArray = None
          noCurves = None
          endShape = None }

[<StringEnum>]
type internal AnchorType =
    | Top
    | Bottom
    | Left
    | Right
    | Middle

type internal Relation =
    { targetId: string
      targetAnchor: AnchorType
      sourceAnchor: AnchorType
      label: ReactElement option
      style: Style option }

type internal ElementProp =
    | Id of string
    | Relations of Relation []

module internal Archer =
    let inline container (props: IHTMLProp seq) children =
        ofImport "ArcherContainer" "react-archer" (keyValueList CaseRules.LowerFirst props) children

    let inline element (props: ElementProp seq) children =
        ofImport "ArcherElement" "react-archer" (keyValueList CaseRules.LowerFirst props) children
