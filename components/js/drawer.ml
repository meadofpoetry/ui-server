open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_tyxml.Drawer.Make(Xml)(Svg)(Html)

type elt =
  [ `Elt of Dom_html.element Js.t
  | `Content of Widget.t list
  ]

module Parent =
  Side_sheet.Make_parent(struct
      include Markup.CSS
      let slide = `Leading
    end)

module Scrim = struct

  class t ?elt () =
    let elt = match elt with
      | Some elt -> elt
      | None -> To_dom.of_element @@ Markup.create_scrim () in
    object
      inherit Widget.t elt ()
    end

  (** Creates new widget from scratch *)
  let make () : t =
    new t ()

  (** Attach widget to existing element *)
  let attach (elt : #Dom_html.element Js.t) : t =
    new t ~elt ()

end

class t (elt : elt) () =
  let elt = match elt with
    | `Elt elt -> elt
    | `Content cnt ->
       let content_wrapper =
         Markup.create_content (List.map Widget.to_markup cnt) () in
       To_dom.of_element @@ Markup.create content_wrapper () in
  object
    inherit Parent.t elt () as super

    method! private focus_active_navigation_item () : unit =
      "." ^ Item_list.Markup.Item.activated_class
      |> Js.string
      |> (fun s -> super#root##querySelector s)
      |> (fun i -> Js.Opt.iter i (fun e -> e##focus))

  end

(** Creates new widget from scratch *)
let make ~(content : #Widget.t list) () : t =
  new t (`Content (List.map Widget.coerce content)) ()

(** Attach widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t =
  new t (`Elt (Element.coerce elt)) ()
