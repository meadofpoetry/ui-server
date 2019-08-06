open Js_of_ocaml
open Components
open Netlib

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

module Attr = struct
  let hidden = "hidden"
  let aria_hidden = "aria-hidden"
end

module Selector = struct
  let tab_bar = Printf.sprintf ".%s .%s" Top_app_bar.CSS.root Tab_bar.CSS.root
  let tabpanel = Printf.sprintf "div[role=\"tabpanel\"]"
  let slider = ".slides"
end

let update_tabpanels elt active =
  let translate = Printf.sprintf "translate(%d%%, 0)" (-100 * active) in
  elt##.style##.transform := Js.string translate;
  List.iteri (fun i x ->
      let hidden = i <> active in
      Element.set_attribute x Attr.aria_hidden (string_of_bool hidden);
      match Element.query_selector x Selector.tabpanel with
      | None -> ()
      | Some x ->
        if hidden then Element.set_attribute x Attr.hidden ""
        else Element.remove_attribute x Attr.hidden)
  @@ Element.children elt

let set_active_page container (tab_bar : Tab_bar.t) =
  let hash = Js.to_string Dom_html.window##.location##.hash in
  let active_tab_id = match String.split_on_char '#' hash with
    | [_; id] -> Some id
    | _ -> None in
  let default () = match tab_bar#tabs with
    | [] -> None
    | x :: _ -> Some x in
  let active = match active_tab_id with
    | None -> default ()
    | Some id ->
      let tab = List.find (fun (tab : Tab.t) ->
          String.equal id (Js.to_string tab#root##.id))
          tab_bar#tabs in
      try Some tab with _ -> default () in
  match active with
  | None -> ()
  | Some tab ->
    update_tabpanels container tab#index;
    if not tab#active then Lwt.async (fun () -> tab_bar#set_active_tab tab)

let update_url_hash hash =
  let history = Dom_html.window##.history in
  let hash = "#" ^ hash in
  history##replaceState Js.undefined (Js.string "") (Js.some @@ Js.string hash)

class t (elt : Dom_html.element Js.t) = object(self)
  val tab_bar : Tab_bar.t =
    match Element.query_selector Dom_html.document##.body Selector.tab_bar with
    | None -> failwith "tab bar element not found"
    | Some x -> Tab_bar.attach x

  val mutable listeners = []

  inherit Widget.t elt () as super

  method! init () : unit =
    set_active_page super#root tab_bar;
    super#init ()

  method! initial_sync_with_dom () : unit =
    listeners <- [
      Tab_bar.Event.changes tab_bar#root self#handle_tab_change
    ];
    super#initial_sync_with_dom ()

  method! destroy () : unit =
    tab_bar#destroy ();
    super#destroy ()

  method private handle_tab_change e _ : unit Lwt.t =
    let tab = (Widget.event_detail e)##.tab in
    update_url_hash (Js.to_string tab##.id);
    set_active_page super#root tab_bar;
    Lwt.return_unit
end

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    Application_http_js.get_topology ()
    >>=? fun _topo ->
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>=? fun socket -> Application_http_js.Event.get_topology socket
    >>=? fun (_, topo_event) ->
    let slides = Element.query_selector_exn
        scaffold#app_content_inner
        Selector.slider in
    let page = new t slides in
    page#set_on_destroy (fun () ->
        React.E.stop ~strong:true topo_event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok page in
  let body = Ui_templates.Loader.create_widget_loader
      ~parent:scaffold#app_content_inner
      thread in
  scaffold#set_body body
