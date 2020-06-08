open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
include Page_timedate_settings_tyxml.Time
module D = Make (Ptime_clock) (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let name = "Time"

module Selector = struct
  let date = "#" ^ date_id

  let time = "#" ^ time_id
end

class t (elt : Dom_html.element Js.t) (mtimer : Mtime.span React.signal)
  (disabled : bool React.signal) =
  let signal, push = React.S.create Ptime.epoch in
  object (self)
    val date : string Textfield.t =
      let date_elt = Element.query_selector_exn elt Selector.date in
      Textfield.attach ~validation:Textfield.Text date_elt

    val time : string Textfield.t =
      let time_elt = Element.query_selector_exn elt Selector.time in
      Textfield.attach ~validation:Textfield.Text time_elt

    inherit Widget.t elt () as super

    val _signal = signal

    val mutable _handlers = []

    val mutable _listeners = []

    val mutable _mtime_base = React.S.value mtimer

    val mutable _set_by_user = false

    method! init () : unit =
      self#handle_time_change;
      super#init ()

    method! initial_sync_with_dom () : unit =
      _listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.
          [
            changes time#input_element (fun _ _ ->
                _set_by_user <- true;
                self#handle_time_change;
                Lwt.return_unit);
            changes date#input_element (fun _ _ ->
                _set_by_user <- true;
                self#handle_time_change;
                Lwt.return_unit);
          ];
      _handlers <-
        [
          React.E.map (fun t -> self#update_time t) @@ React.S.changes mtimer;
          React.E.map (fun flag -> self#set_disabled flag)
          @@ React.S.changes disabled;
        ];
      self#set_disabled (React.S.value disabled);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      time#destroy ();
      date#destroy ();
      List.iter (React.E.stop ~strong:true) _handlers;
      super#destroy ()

    method update_time span =
      let open Mtime.Span in
      let secs = to_s span -. to_s _mtime_base in
      let time =
        Ptime.add_span self#value
          (Option.value ~default:Ptime.Span.zero (Ptime.Span.of_float_s secs))
      in
      Option.iter self#local_set_value time

    method value = React.S.value _signal

    method set_value t =
      _set_by_user <- false;
      _mtime_base <- React.S.value mtimer;
      self#local_set_value t

    method set_by_user = _set_by_user

    method private handle_time_change =
      try
        let y, m, d =
          Scanf.sscanf (Option.get date#value) "%04d-%02d-%02d" (fun y m d ->
              (y, m, d))
        in
        let hours, minutes =
          Scanf.sscanf (Option.get time#value) "%02d:%02d" (fun h m -> (h, m))
        in
        let tz_off =
          Option.value ~default:0 (Ptime_clock.current_tz_offset_s ())
        in
        match Ptime.of_date_time ((y, m, d), ((hours, minutes, 0), tz_off)) with
        | None -> ()
        | Some t ->
            _mtime_base <- React.S.value mtimer;
            push t
      with _ -> print_endline "Date or time is null"

    method private local_set_value t =
      let (y, m, d), ((hours, minutes, _), _) =
        Ptime.to_date_time ?tz_offset_s:(Ptime_clock.current_tz_offset_s ()) t
      in
      let date_str = Printf.sprintf "%04d-%02d-%02d" y m d in
      let time_str = Printf.sprintf "%02d:%02d" hours minutes in
      time#set_value time_str;
      date#set_value date_str

    method private set_disabled flag =
      time#set_disabled flag;
      date#set_disabled flag
  end

let make (init : Pc_control_types.Timedate_config.t)
    (mtimer : Mtime.span React.signal) (disabled : bool React.signal) : t =
  let open Pc_control_types.Timedate_config in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element @@ D.create init
  in
  new t elt mtimer disabled
