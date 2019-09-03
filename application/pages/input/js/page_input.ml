open Js_of_ocaml
open Components

let ( >>= ) = Lwt.bind

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
      try Some (List.find (fun (tab : Tab.t) ->
          String.equal id (Js.to_string tab#root##.id))
          tab_bar#tabs)
      with _ -> default () in
  match active with
  | None -> ()
  | Some tab ->
    update_tabpanels container tab#index;
    if not tab#active then Lwt.async (fun () -> tab_bar#set_active_tab tab)

let update_url_hash hash =
  let history = Dom_html.window##.history in
  let hash = "#" ^ hash in
  history##replaceState Js.undefined (Js.string "") (Js.some @@ Js.string hash)

let attach_tab_bar () =
  match Element.query_selector Dom_html.document##.body Selector.tab_bar with
  | None -> failwith "tab bar element not found"
  | Some x -> Tab_bar.attach x

let handle_tab_change body tab_bar e _ : unit Lwt.t =
  let tab = (Widget.event_detail e)##.tab in
  update_url_hash (Js.to_string tab##.id);
  set_active_page body tab_bar;
  Lwt.return_unit

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let get_body () = match scaffold#body with
    | None -> Lwt.fail_with "page body element not found"
    | Some x -> Lwt.return x in
  let thread =
    scaffold#loaded
    >>= get_body
    >>= fun body ->
    let tab_bar = attach_tab_bar () in
    let changes =
      Tab_bar.Event.changes
        tab_bar#root
        (handle_tab_change body tab_bar) in
    tab_bar#set_on_destroy (fun () -> Lwt.cancel changes);
    set_active_page body tab_bar;
    Lwt.return_ok ()
  in
  let (_ : Dom_html.element Js.t) =
    Components_lab.Loader.make_loader
      ~elt:scaffold#app_content_inner
      thread
  in
  ()
