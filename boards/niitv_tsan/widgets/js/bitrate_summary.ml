open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Bitrate_summary
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

type event = [`Bitrate of Bitrate.t option]

module Selector = struct
  let total =
    Printf.sprintf ".%s.%s .%s" CSS.value CSS.value_total Item_list.CSS.item_meta

  let effective =
    Printf.sprintf ".%s.%s .%s" CSS.value CSS.value_effective Item_list.CSS.item_meta
end

class t (elt : Dom_html.element Js.t) =
  object (self)
    inherit Widget.t elt ()

    val total = Element.query_selector_exn elt Selector.total

    val effective = Element.query_selector_exn elt Selector.effective

    method notify : event -> unit =
      function
      | `Bitrate x -> self#set_rate x

    method private set_rate =
      function
      | None ->
          self#set_total None;
          self#set_effective None
      | Some (x : Bitrate.t) ->
          let null = Option.value ~default:0 (List.assoc_opt 0x1FFF x.pids) in
          let e = x.total - null in
          let e = Float.(of_int e /. 1_000_000.) in
          let v = Float.(of_int x.total /. 1_000_000.) in
          self#set_total (Some v);
          self#set_effective (Some e)

    method private set_total (x : float option) : unit =
      total##.textContent := Js.some @@ Js.string (bitrate_to_string x)

    method private set_effective (x : float option) : unit =
      effective##.textContent := Js.some @@ Js.string (bitrate_to_string x)
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (elt :> Dom_html.element Js.t)

let make ?classes ?attrs ?total ?effective ?children () =
  Markup_js.create ?classes ?attrs ?total ?effective ?children ()
  |> Tyxml_js.To_dom.of_ul
  |> attach
