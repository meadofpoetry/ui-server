open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_http_js
include Board_niitv_tsan_widgets_tyxml.Table_overview
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>= ) = Lwt.bind

module Attr = struct
  let data_control = "data-control"

  let data_mode = "data-mode"
end

module Selector = struct
  let menu_icon = "." ^ CSS.menu_icon

  let placeholder = Printf.sprintf ".%s" Components_lab.Placeholder.CSS.root

  let table = Printf.sprintf ".%s" CSS.table

  let menu = "." ^ Menu.CSS.root
end

class virtual ['a] t ~format elt () =
  object (self)
    val control : int =
      match Element.get_attribute elt Attr.data_control with
      | None ->
          failwith
            (Printf.sprintf
               "%s: no `%s` attribute found on root element"
               CSS.root
               Attr.data_control)
      | Some x -> int_of_string x

    val menu : Menu.t option =
      Option.map Menu.attach @@ Element.query_selector elt Selector.menu

    val menu_icon : Icon_button.t option =
      Option.map Icon_button.attach @@ Element.query_selector elt Selector.menu_icon

    val placeholder =
      match Element.query_selector elt Selector.placeholder with
      | Some x -> x
      | None -> Tyxml_js.To_dom.of_div @@ Markup_js.create_empty_placeholder ()

    val table : _ Gadt_data_table.t =
      Gadt_data_table.attach ~fmt:format @@ Element.query_selector_exn elt Selector.table

    val mutable hex = false

    val mutable listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      self#update_empty_state ();
      Option.iter (fun menu -> menu#set_quick_open true) menu;
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <-
        (match menu, menu_icon with
        | Some menu, Some menu_icon ->
            [ Js_of_ocaml_lwt.Lwt_js_events.clicks menu_icon#root (fun _ _ ->
                  menu#reveal ())
            ; Menu.Lwt_js_events.selects menu#root self#handle_menu_selection_change ]
        | _ -> [])
        @ listeners;
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      table#destroy ();
      Option.iter Widget.destroy menu;
      Option.iter Widget.destroy menu_icon;
      List.iter Lwt.cancel listeners;
      super#destroy ()

    method set_state (state : [Application_types.Topology.state | `No_sync]) =
      let no_sync, no_response =
        match state with
        | `Fine -> false, false
        | `No_sync -> true, false
        | `Detect | `Init | `No_response -> false, true
      in
      Element.toggle_class_unit ~force:no_sync super#root CSS.no_sync;
      Element.toggle_class_unit ~force:no_response super#root CSS.no_response
    (** Updates widget state *)

    method virtual private set_hex : bool -> unit

    method virtual private find_row : 'a -> Dom_html.tableRowElement Js.t option

    method private remove_row (id : 'a) =
      match self#find_row id with
      | None -> ()
      | Some row -> table#table##deleteRow row##.rowIndex

    method private reset_bitrate_stats () : unit Lwt.t =
      let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
      let rec aux () =
        Http_monitoring.reset_bitrate_stats control
        >>= function
        | Ok () ->
            let snackbar =
              Snackbar.make
                ~label:(`Text "Статистика битрейта сброшена")
                ~dismiss:`True
                ()
            in
            scaffold#show_snackbar ~on_close:(fun _ -> snackbar#destroy ()) snackbar
        | Error (`Msg msg) ->
            let snackbar =
              Snackbar.make
                ~label:
                  (`Text
                    (Printf.sprintf
                       "Не удалось сбросить статистику \
                        битрейта. %s"
                       msg))
                ~action:(`Text "Повторить")
                ()
            in
            scaffold#show_snackbar
              ~on_close:(fun reason ->
                (match reason with
                | Action -> Lwt.async aux
                | _ -> ());
                snackbar#destroy ())
              snackbar
      in
      aux ()

    method private handle_menu_selection_change e _ : unit Lwt.t =
      let detail = Widget.event_detail e in
      if Element.has_class detail##.item CSS.bitrate_reset
      then self#reset_bitrate_stats ()
      else (
        (match Element.get_attribute detail##.item Attr.data_mode with
        | Some "hex" ->
            hex <- true;
            self#set_hex hex
        | Some "dec" ->
            hex <- false;
            self#set_hex hex
        | _ -> ());
        Lwt.return_unit)

    method private update_empty_state () =
      if table#rows_collection##.length = 0
      then Dom.appendChild super#root placeholder
      else Element.remove placeholder
  end
