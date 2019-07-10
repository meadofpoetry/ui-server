open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Pipeline_types

include Page_mosaic_editor_tyxml.List_of_widgets
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Selector = struct
  let item = Printf.sprintf ".%s" CSS.item
end

let format = "application/json"

let id_prefix = "add"

let splitter = '-'

let make_id (s : string) =
  Printf.sprintf "%s%c%s" id_prefix splitter s

let parse_id (s : string) =
  match String.split_on_char splitter s with
  | [_; id] -> id
  | _ -> s

class t ?drag_image (elt : Dom_html.element Js.t) = object(self)
  inherit Widget.t elt () as super
  val placeholder = Ui_templates.Placeholder.With_icon.make
      ~text:"Нет доступных виджетов"
      ~icon:Icon.SVG.(make_simple Path.information)#root
      ()
  val mutable _listeners = []
  val mutable _drag_target = Js.null

  method! init () : unit =
    _listeners <- Lwt_js_events.(
        [ dragstarts super#root self#handle_dragstart
        ; dragends super#root self#handle_dragend
        ]);
    super#init ()

  method! initial_sync_with_dom () : unit =
    (match self#items with
     | [] -> Dom.appendChild super#root placeholder#root
     | _ -> Element.remove_child_safe super#root placeholder#root);
    super#initial_sync_with_dom ()

  method! destroy () : unit =
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    super#destroy ()

  method items : Dom_html.element Js.t list =
    Element.query_selector_all super#root Selector.item

  method remove_by_id (id : string) =
    match Dom_html.getElementById_opt (make_id id) with
    | None -> ()
    | Some x -> Dom.removeChild super#root x

  method append_item (id, w : string * Wm.widget) =
    let elt =
      Tyxml_js.To_dom.of_element
      @@ Markup.create_item ~id:(make_id id) w in
    Element.remove_child_safe super#root placeholder#root;
    Dom.appendChild super#root elt

  method private handle_dragstart e _ =
    let target = Dom_html.eventTarget e in
    _drag_target <- e##.target;
    let to_yojson (id, w) : Yojson.Safe.t =
      `List [ `String (parse_id id)
            ; Wm.widget_to_yojson w ] in
    let data =
      Js.string
      @@ Yojson.Safe.to_string
      @@ to_yojson
      @@ Wm_widget.of_element target in
    e##.dataTransfer##setData (Js.string format) data;
    target##.style##.opacity := Js.def @@ Js.string "0.5";
    target##.style##.zIndex := Js.string "5";
    Lwt.return_unit

  method private handle_dragend e _ =
    let target = Dom_html.eventTarget e in
    target##.style##.opacity := Js.def @@ Js.string "";
    target##.style##.zIndex := Js.string "";
    if Element.has_class target Item_list.CSS.item
    && (not (Js.some target == _drag_target))
    then (Dom.preventDefault e;
          e##.dataTransfer##.dropEffect := Js.string "move");
    Lwt.return_unit
end

module Domains = Map.Make(struct
    type t = Wm.domain
    let compare = compare
  end)

let domain_to_string : Wm.domain -> string = function
  | Wm.Nihil -> "Без группы"
  | Chan { stream; channel } ->
    Printf.sprintf "Поток %s, Канал %d"
      (Application_types.Stream.ID.to_string stream)
      channel

let group_by_domain (widgets : (string * Wm.widget) list) =
  let map = Domains.empty in
  List.fold_left (fun acc (((id, w) as x) : string * Wm.widget) ->
      Domains.update w.domain (function
          | None -> Some [x]
          | Some l -> Some (x :: l)) acc)
    map widgets

let make ?drag_image (widgets : (string * Wm.widget) list) : t =
  let grouped = group_by_domain widgets in
  let items = Domains.map (List.map (fun (id, x) ->
      Markup.create_item ~id:(make_id id) x)) grouped in
  let content =
    List.concat
    @@ List.map (fun (domain, items) ->
        [ Markup.create_subheader (domain_to_string domain)
        ; Markup.create_list items
        ])
    @@ Domains.bindings items in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create content in
  new t ?drag_image elt
