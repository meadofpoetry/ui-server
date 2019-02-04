open Utils
open Containers

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module Item_list = Item_list.Make(Xml)(Svg)(Html)

  let base_class = "mdc-menu"
  let items_class = CSS.add_element base_class "items"
  let anchor_class = "mdc-menu-anchor"

  module Item = struct

    include Item_list.Item

    let create ?classes ?attrs ?tag ?graphic ?meta
          ?(disabled = false) text () : 'a elt =
      create ?classes ?tag ?graphic ?meta
        ~attrs:([a_role ["menuitem"]]
                |> cons_if disabled @@ a_aria "disabled" ["true"]
                |> cons_if disabled @@ a_tabindex (-1)
                |> cons_if (not disabled) @@ a_tabindex 0
                |> (fun x -> x @ (Option.get_or ~default:[] attrs)))
        text ()

  end

  let create_list ?(classes = []) ?attrs ~items () : 'a elt =
    Item_list.create ~classes:([items_class] |> (fun x -> x @ classes))
      ~attrs:([ a_role ["menu"]
              ; a_aria "hidden" ["true"] ]
              |> (fun x -> x @ (Option.get_or ~default:[] attrs)))
      ~items ()

  let create ?(classes = []) ?attrs ?(opened = false)
        ?open_from ~list () : 'a elt =
    let classes =
      classes
      |> cons_if opened @@ CSS.add_modifier base_class "open"
      |> map_cons_option (fun x ->
             (match x with
              | `Top_left     -> "top-left"
              | `Top_right    -> "top-right"
              | `Bottom_left  -> "bottom-left"
              | `Bottom_right -> "bottom-right")
             |> (fun s -> CSS.add_modifier base_class ("open-from-" ^ s)))
           open_from
      |> List.cons base_class in
    div ~a:([a_class classes; a_tabindex (-1)] <@> attrs) [list]

end
