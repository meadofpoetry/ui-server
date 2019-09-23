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
end

class t ?set_hex elt () =
  object (self)
    val mutable service_info = None

    inherit [int] Table_overview.with_details ?set_hex elt () as super

    method! init () : unit =
      service_info <- Some (Service_info.make ~control ());
      super#init ()

    method service_info : Service_info.t = Option.get service_info

    method private get_row_title (_row : Dom_html.tableRowElement Js.t) = ""

    (* match table#get_row_data_lazy row with
     * | _ :: name :: _ -> name () *)
    method private handle_row_action (row : Dom_html.tableRowElement Js.t) =
      let id = Option.bind (Element.get_attribute row Attr.data_id) int_of_string_opt in
      let info =
        match Element.get_attribute row Attr.data_value with
        | None -> None
        | Some x -> (
          match Service.of_yojson @@ Yojson.Safe.from_string x with
          | Ok x -> Some x
          | Error _ -> None)
      in
      match id, info with
      | Some id, Some info ->
          (self#service_info)#notify (`Service (Some (id, info)));
          Js_of_ocaml_lwt.Lwt_js_events.click back_action#root
          >>= fun _ ->
          Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame ()
          >>= fun () -> Lwt.return_unit
      | _ -> Lwt.return_unit
  end

let attach ?set_hex elt : t = new t ?set_hex (elt : Dom_html.element Js.t) ()
