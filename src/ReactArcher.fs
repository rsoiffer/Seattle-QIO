module ReactArcher

open Fable.Core
open Fable.Core.JsInterop
open Fable.React

type ArcherStyle =
    { strokeColor: string option
      strokeWidth: int option
      strokeDashArray: int option
      noCurves: bool option
      endShape: obj option }

module ArcherStyle =
    let defaults =
        { strokeColor = None
          strokeWidth = None
          strokeDashArray = None
          noCurves = None
          endShape = None }

[<StringEnum>]
type AnchorType =
    | Top
    | Bottom
    | Left
    | Right
    | Middle

type Relation =
    { targetId: string
      targetAnchor: AnchorType
      sourceAnchor: AnchorType
      label: ReactElement option
      style: ArcherStyle option }

type ArcherElementProps =
    | Id of string
    | Relations of Relation[]

let inline archerContainer props children =
    ofImport "ArcherContainer" "react-archer" (keyValueList CaseRules.LowerFirst props) children

let inline archerElement (props: ArcherElementProps list) children =
    ofImport "ArcherElement" "react-archer" (keyValueList CaseRules.LowerFirst props) children
