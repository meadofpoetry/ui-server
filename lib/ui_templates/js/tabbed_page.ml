open Js_of_ocaml
open Components

let ( >>= ) = Lwt.bind

module Attr = struct
  let hidden = "hidden"
end

module Selector = struct
  let tab_bar = Printf.sprintf ".%s .%s" Top_app_bar.CSS.root Tab_bar.CSS.root

  let tabpanel = Printf.sprintf "div[role=\"tabpanel\"]"

  let tabpanel_content = "." ^ Ui_templates_tyxml.Tabbed_page.CSS.tabpanel_content

  let glide = "." ^ Components_lab.Glide.CSS.root
end

(* Tabpanel *)

type state = < finalize : unit -> unit >

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
      if x##.oldValue != current
      then
        Js.Opt.case
          current
          (fun () ->
            match on_visible with
            | None -> ()
            | Some f -> state_ref := f ())
          (fun _ ->
            match !state_ref with
            | None -> ()
            | Some state ->
                state#finalize ();
                state_ref := None)

let init_tabpanel ?on_visible id =
  match Dom_html.getElementById_opt id with
  | None -> Lwt.return_error (Printf.sprintf "tabpanel with id `%s` not found" id)
  | Some elt ->
      let state_ref : state option ref = ref None in
      let node = Js.Opt.get (Element.get_parent elt) (fun () -> assert false) in
      let observer =
        MutationObserver.observe
          ~node
          ~f:(fun records _ -> callback state_ref records)
          ~attributes:true
          ~attribute_old_value:true
          ~attribute_filter:[Js.string Attr.hidden]
          ()
      in
      Js.Opt.iter
        (node##getAttribute (Js.string Attr.hidden))
        (fun _ -> Option.iter (fun f -> state_ref := f ()) on_visible);
      Lwt.return_ok (elt, observer)

(* Page *)

let hd_opt = function
  | [] -> None
  | hd :: _ -> Some hd

let get_active_tab tab_bar =
  let hash = Js.to_string Dom_html.window##.location##.hash in
  let active_tab_id =
    match String.split_on_char '#' hash with
    | [_; id] -> Some id
    | _ -> None
  in
  match active_tab_id with
  | None -> hd_opt tab_bar#tabs
  | Some id -> (
    try
      Some
        (List.find
           (fun (tab : Tab.t) -> String.equal id (Js.to_string tab#root##.id))
           tab_bar#tabs)
    with _ -> hd_opt tab_bar#tabs)

let set_active_page state_ref (tab_bar : Tab_bar.t) (glide : Components_lab.Glide.t) =
  match get_active_tab tab_bar with
  | None -> Lwt.return_unit
  | Some tab ->
      let active = tab#index in
      List.iteri
        (fun i item ->
          let content = Element.query_selector_exn item Selector.tabpanel_content in
          if i <> active
          then Element.set_attribute content Attr.hidden ""
          else Element.remove_attribute content Attr.hidden)
        glide#items;
      glide#set_active tab#index;
      if not tab#active then Lwt.async (fun () -> tab_bar#set_active_tab tab);
      (* Cancel previous loading *)
      Option.iter Lwt.cancel !state_ref;
      (* TODO page loading must be handled here *)
      state_ref := None;
      Lwt.return_unit

let update_url_hash hash =
  let history = Dom_html.window##.history in
  let hash = "#" ^ hash in
  history##replaceState Js.undefined (Js.string "") (Js.some @@ Js.string hash)

let handle_tab_change state_ref body tab_bar e _ : unit Lwt.t =
  let tab = (Widget.event_detail e)##.tab in
  update_url_hash (Js.to_string tab##.id);
  set_active_page state_ref body tab_bar

let init : unit =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let state_ref = ref None in
  let thread =
    scaffold#loaded
    >>= fun () ->
    let tab_bar =
      Tab_bar.attach
      @@ Element.query_selector_exn Dom_html.document##.body Selector.tab_bar
    in
    let glide =
      Components_lab.Glide.attach
      @@ Element.query_selector_exn scaffold#app_content_inner Selector.glide
    in
    let listeners =
      [ Tab_bar.Lwt_js_events.changes
          tab_bar#root
          (handle_tab_change state_ref tab_bar glide) ]
    in
    scaffold#set_on_destroy (fun () ->
        tab_bar#destroy ();
        glide#destroy ();
        List.iter Lwt.cancel listeners);
    set_active_page state_ref tab_bar glide >>= fun () -> Lwt.return_ok ()
  in
  ignore (Components_lab.Loader.make_loader ~elt:scaffold#app_content_inner thread)
