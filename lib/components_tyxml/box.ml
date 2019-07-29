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
    BEM.add_modifier root
      (match x with
       | `Nowrap -> "nowrap"
       | `Wrap -> "wrap"
       | `Wrap_reverse -> "wrap-reverse")

  let justify_content (x : justify_content) : string =
    (BEM.add_modifier root "justify-content-")
    ^ (match x with
       | `Start -> "start"
       | `End -> "end"
       | `Center -> "center"
       | `Space_between -> "space-between"
       | `Space_around -> "space-around"
       | `Space_evenly -> "space-evenly")

  let align_items (x : align_items) : string =
    (BEM.add_modifier root "align-items-")
    ^ (match x with
       | `Start -> "start"
       | `End -> "end"
       | `Center -> "center"
       | `Stretch -> "stretch"
       | `Baseline -> "baseline")

  let align_content (x : align_content) : string =
    (BEM.add_modifier root "align-content-")
    ^ (match x with
       | `Start -> "start"
       | `End -> "end"
       | `Center -> "center"
       | `Stretch -> "stretch"
       | `Space_between -> "space-between"
       | `Space_around -> "space-around")

end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create ?(classes = []) ?(attrs = []) ?tag
        ?justify_content ?align_items ?align_content ?wrap
        ?(vertical = false) ~content () : 'a elt =
    let tag = match tag with
      | None -> div
      | Some x -> x in
    let classes =
      classes
      |> cons_if vertical CSS.vertical
      |> map_cons_option CSS.wrap wrap
      |> map_cons_option CSS.justify_content justify_content
      |> map_cons_option CSS.align_items align_items
      |> map_cons_option CSS.align_content align_content
      |> List.cons CSS.root in
    tag ~a:([a_class classes] @ attrs) content

end
