type justify_content =
  [ `Start
  | `End
  | `Center
  | `Space_between
  | `Space_around
  | `Space_evenly ]

type align_items =
  [ `Start
  | `End
  | `Center
  | `Stretch
  | `Baseline ]

type align_content =
  [ `Start
  | `End
  | `Center
  | `Stretch
  | `Space_between
  | `Space_around ]

type wrap =
  [ `Nowrap
  | `Wrap
  | `Wrap_reverse ]

module CSS = struct
  let root = "mdc-box"

  let vertical = BEM.add_modifier root "vertical"

  let horizontal = BEM.add_modifier root "horizontal"

  let wrap (x : wrap) : string =
    let modifier =
      match x with
      | `Nowrap -> "nowrap"
      | `Wrap -> "wrap"
      | `Wrap_reverse -> "wrap-reverse"
    in
    BEM.add_modifier root modifier

  let justify_content (x : justify_content) : string =
    let suffix =
      match x with
      | `Start -> "start"
      | `End -> "end"
      | `Center -> "center"
      | `Space_between -> "space-between"
      | `Space_around -> "space-around"
      | `Space_evenly -> "space-evenly"
    in
    BEM.add_modifier root "justify-content-" ^ suffix

  let align_items (x : align_items) : string =
    let suffix =
      match x with
      | `Start -> "start"
      | `End -> "end"
      | `Center -> "center"
      | `Stretch -> "stretch"
      | `Baseline -> "baseline"
    in
    BEM.add_modifier root "align-items-" ^ suffix

  let align_content (x : align_content) : string =
    let suffix =
      match x with
      | `Start -> "start"
      | `End -> "end"
      | `Center -> "center"
      | `Stretch -> "stretch"
      | `Space_between -> "space-between"
      | `Space_around -> "space-around"
    in
    BEM.add_modifier root "align-content-" ^ suffix
end

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let box
      ?(classes = return [])
      ?(a = [])
      ?tag
      ?justify_content
      ?align_items
      ?align_content
      ?wrap
      ?(vertical = false)
      ?(children = nil ())
      () =
    let tag =
      match tag with
      | None -> div
      | Some x -> x
    in
    let classes =
      fmap
        (Utils.cons_if vertical CSS.vertical
        % Utils.map_cons_option CSS.wrap wrap
        % Utils.map_cons_option CSS.justify_content justify_content
        % Utils.map_cons_option CSS.align_items align_items
        % Utils.map_cons_option CSS.align_content align_content
        % List.cons CSS.root)
        classes
    in
    tag ~a:(a_class classes :: a) children
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
