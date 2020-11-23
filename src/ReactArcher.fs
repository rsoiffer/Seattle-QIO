module ReactArcher

open Fable.Core
open Fable.Core.JsInterop
open Fable.React

type ArcherStyle =
    | StrokeColor of string
    | StrokeWidth of int
    | StrokeDashArray of int
    | NoCurves of bool
    | EndShape of obj

[<StringEnum>]
type AnchorType =
    | Top
    | Bottom
    | Left
    | Right
    | Middle

type Relation =
    | TargetId of string
    | TargetAnchor of AnchorType
    | SourceAnchor of AnchorType
    | Label of ReactElement
    | Style of ArcherStyle list

type ArcherElementProps =
    | Id of string
    | Relations of Relation array

let inline archerContainer props children =
    ofImport "ArcherContainer" "react-archer" (keyValueList CaseRules.LowerFirst props) children

let inline archerElement (props: ArcherElementProps list) children =
    ofImport "ArcherElement" "react-archer" (keyValueList CaseRules.LowerFirst props) children
