open Js_of_ocaml
open Components
open Components_lab

let ( >>= ) = Lwt.bind

let ( >>=? ) = Lwt_result.bind

module Attr = struct
  let hidden = "hidden"
end

module Selector = struct
  let tab_bar = Printf.sprintf ".%s .%s" Top_app_bar.CSS.root Tab_bar.CSS.root

  let tabpanel_content =
    "." ^ Ui_templates_tyxml.Tabbed_page.CSS.tabpanel_content

  let glide = "." ^ Components_lab.Glide.CSS.root
end

module Tabpanel = struct
  let callback ?on_visible state_ref records =
    let open MutationObserver in
    let record =
      List.find_opt (fun (x : mutationRecord Js.t) ->
          x##._type == Js.string "attributes"
          && x##.attributeName == Js.some @@ Js.string Attr.hidden)
      @@ Array.to_list
      @@ Js.to_array records
    in
    match record with
    | None -> ()
    | Some x ->
        let (target : Dom_html.element Js.t) = Js.Unsafe.coerce x##.target in
        let current = target##getAttribute (Js.string "hidden") in
        if x##.oldValue != current then
          Js.Opt.case current
            (fun () ->
              match on_visible with None -> () | Some f -> state_ref := f ())
            (fun _ ->
              match !state_ref with
              | None -> ()
              | Some state ->
                  state#finalize ();
                  state_ref := None)

  let init ?on_visible ?on_hidden ~id () =
    let tab_id = Ui_templates_tyxml.Tabbed_page.tab_id id in
    let tabpanel_id = Ui_templates_tyxml.Tabbed_page.tabpanel_id id in
    let body = Dom_html.document##.body in
    match Dom_html.getElementById_opt tabpanel_id with
    | None ->
        Error
          (`Msg (Printf.sprintf "tabpanel with id `%s` not found" tabpanel_id))
    | Some tabpanel -> (
        match Element.query_selector tabpanel Selector.tabpanel_content with
        | None ->
            Error (`Msg (Printf.sprintf "tabpanel content element not found"))
        | Some content ->
            let listener =
              Option.map
                (fun tab_bar ->
                  Tab_bar.Lwt_js_events.changes tab_bar (fun e _ ->
                      let detail = Widget.event_detail e in
                      let previous =
                        Js.Opt.map detail##.previousTab (fun x -> x##.id)
                      in
                      if detail##.tab##.id == Js.string tab_id then
                        Option.iter (fun f -> f content) on_visible
                      else if previous == Js.some (Js.string tab_id) then
                        Option.iter (fun f -> f content) on_hidden;
                      Lwt.return_unit))
                (Element.query_selector body Selector.tab_bar)
            in
            Ok (content, fun () -> Option.iter Lwt.cancel listener) )
end

let get_active_tab (tab_bar : Tab_bar.t) =
  let hash = Js.to_string Dom_html.window##.location##.hash in
  let tab_id =
    match String.split_on_char '#' @@ hash with
    | [ _; id ] -> Some id
    | _ -> None
  in
  let active_by_hash =
    Option.bind tab_id (fun id ->
        List.find_opt
          (fun (tab : Tab.t) -> String.equal id (Js.to_string tab#root##.id))
          tab_bar#tabs)
  in
  match active_by_hash with
  | Some _ as x -> x
  | None -> ( match tab_bar#tabs with [] -> None | hd :: _ -> Some hd )

let set_active_page (glide : Components_lab.Glide.t)
    (tab : Tab_bar.Event.detail Js.t) =
  let active = tab##.index in
  List.iteri
    (fun i item ->
      let content = Element.query_selector_exn item Selector.tabpanel_content in
      if i <> active then Element.set_attribute content Attr.hidden ""
      else Element.remove_attribute content Attr.hidden)
    glide#items;
  glide#set_active active;
  Lwt.return_unit

let update_url_hash hash =
  let history = Dom_html.window##.history in
  let hash = "#" ^ hash in
  history##replaceState Js.undefined (Js.string "") (Js.some @@ Js.string hash)

let handle_tab_change glide e _ : unit Lwt.t =
  let detail = Widget.event_detail e in
  update_url_hash (Js.to_string detail##.tab##.id);
  set_active_page glide detail

let query_selector_lwt elt selector =
  try Lwt.return_ok @@ Element.query_selector_exn elt selector
  with Failure x -> Lwt.return_error (`Msg x)

let init () =
  Js_of_ocaml_lwt.Lwt_js_events.onload () >>= fun _ ->
  let body = Dom_html.document##.body in
  query_selector_lwt body Selector.tab_bar >>=? fun tab_bar_elt ->
  query_selector_lwt body Selector.glide >>=? fun glide_elt ->
  let tab_bar = Tab_bar.attach tab_bar_elt in
  let glide = Glide.attach glide_elt in
  let listener =
    Tab_bar.Lwt_js_events.changes tab_bar_elt (handle_tab_change glide)
  in
  let thread =
    match get_active_tab tab_bar with
    | None -> Lwt.return ()
    | Some x -> tab_bar#set_active_tab x
  in
  thread >>= fun () ->
  let fin =
    object
      method finalize () =
        tab_bar#destroy ();
        glide#destroy ();
        Lwt.cancel listener
    end
  in
  Lwt.return_ok fin
