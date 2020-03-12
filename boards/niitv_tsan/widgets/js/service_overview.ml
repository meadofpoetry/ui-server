open Js_of_ocaml
open Components
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Service_overview
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

let ( >>= ) = Lwt.bind

module Attr = struct
  let data_id = "data-id"

  let data_value = "data-value"
end

module Selector = struct
  let icon_button = Printf.sprintf ".%s" Icon_button.CSS.root

  let service_info = Printf.sprintf ".%s" Service_info.CSS.root
end

class t ?set_hex ?on_row_action (elt : Dom_html.element Js.t) () =
  object (self)
    val mutable service_info =
      match Element.query_selector elt Selector.service_info with
      | None -> None
      | Some x -> Some (Service_info.attach x)

    inherit [int] Table_overview.with_details ?set_hex elt () as super

    method! init () : unit =
      self#init_service_info ();
      super#init ()

    method! destroy () : unit =
      Option.iter Widget.destroy service_info;
      super#destroy ()

    method private handle_row_action (row : Dom_html.tableRowElement Js.t) =
      let id =
        Option.bind (Element.get_attribute row Attr.data_id) int_of_string_opt
      in
      match id with
      | Some id ->
          Option.iter (fun f -> f (Some id)) on_row_action;
          (* This is needed to initialize service info widget if it is the first
             time it is added to the DOM. *)
          self#init_service_info ();
          Js_of_ocaml_lwt.Lwt_js_events.click back_action#root >>= fun _ ->
          Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame () >>= fun () ->
          Option.iter (fun f -> f None) on_row_action;
          Lwt.return_unit
      | _ -> Lwt.return_unit

    method private init_service_info () =
      match service_info with
      | Some _ -> ()
      | None -> (
          match Element.query_selector elt Selector.service_info with
          | None -> ()
          | Some x -> service_info <- Some (Service_info.attach x) )
  end

let attach ?set_hex ?on_row_action elt : t =
  new t ?set_hex ?on_row_action (elt : Dom_html.element Js.t) ()
