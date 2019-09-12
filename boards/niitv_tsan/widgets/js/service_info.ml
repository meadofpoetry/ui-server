open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Service_info
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

type event =
  [ `Bitrate of Bitrate.t option
  | `Service of (int * Service.t) option
  | `PIDs of (int * PID.t) list ts ]

module Selector = struct
  let glide = "." ^ Components_lab.Glide.CSS.root

  let tab_bar = "." ^ Tab_bar.CSS.root

  let general_info = "." ^ Service_general_info.CSS.root

  let sdt_info = "." ^ Service_sdt_info.CSS.root

  let pid_overview = "." ^ Pid_overview.CSS.root
end

class t (elt : Dom_html.element Js.t) () =
  object (self)
    val tab_bar : Tab_bar.t =
      Tab_bar.attach @@ Element.query_selector_exn elt Selector.tab_bar

    val glide : Components_lab.Glide.t =
      Components_lab.Glide.attach @@ Element.query_selector_exn elt Selector.glide

    val general_info : Service_general_info.t =
      Service_general_info.attach @@ Element.query_selector_exn elt Selector.general_info

    val sdt_info : Service_sdt_info.t =
      Service_sdt_info.attach @@ Element.query_selector_exn elt Selector.sdt_info

    val pid_overview : Pid_overview.t =
      Pid_overview.attach @@ Element.query_selector_exn elt Selector.pid_overview

    val mutable listeners = []

    inherit Widget.t elt () as super

    method! init () : unit = super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <- [Tab_bar.Lwt_js_events.changes tab_bar#root self#handle_tab_bar_change];
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      tab_bar#destroy ();
      glide#destroy ();
      general_info#destroy ();
      sdt_info#destroy ();
      pid_overview#destroy ();
      super#destroy ()

    method notify : event -> unit =
      function
      | `PIDs _ as x -> pid_overview#notify x
      | `Service _ as x ->
          general_info#notify x;
          sdt_info#notify x
      | `Bitrate _ as x ->
          general_info#notify x;
          pid_overview#notify x

    method private handle_tab_bar_change e _ : unit Lwt.t =
      let detail = Widget.event_detail e in
      glide#set_active detail##.index;
      Lwt.return_unit
  end

let attach elt = new t (elt :> Dom_html.element Js.t) ()

let make ?classes ?attrs ?children () =
  Markup_js.create ?classes ?attrs ?children () |> Tyxml_js.To_dom.of_div |> attach
