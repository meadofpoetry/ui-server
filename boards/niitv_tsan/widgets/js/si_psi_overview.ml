open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_http_js
include Board_niitv_tsan_widgets_tyxml.Si_psi_overview
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module Markup_r = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

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

class t elt () =
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
      Gadt_data_table.attach ~fmt:(Markup_js.create_table_format ~hex:false ())
      @@ Element.query_selector_exn elt Selector.table

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

    method private update_empty_state () =
      if table#rows_collection##.length = 0
      then Dom.appendChild super#root placeholder
      else Element.remove placeholder

    method private set_hex (hex : bool) : unit =
      let pid_fmt = Markup_js.pid_fmt ~hex in
      let (format : _ Markup_js.Fmt.data_format) =
        match table#data_format with
        | _ :: tl -> pid_fmt :: tl
      in
      table#set_data_format ~redraw:false format;
      List.iter
        (fun row ->
          let cells = row##.cells in
          Js.Opt.iter
            (cells##item 0)
            (fun cell ->
              Gadt_data_table.(set_cell_value pid_fmt (get_cell_value pid_fmt cell) cell)))
        table#rows

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
  end

let attach elt : t = new t (elt :> Dom_html.element Js.t) ()
