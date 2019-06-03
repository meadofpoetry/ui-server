open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_http_js
open Components

let base_class = "qos-niit-t2mi-settings"

let make_enabled () =
  let event, push = React.E.create () in
  let enabled = Switch.make ~on_change:(fun x ->
      push (); Lwt.return_unit) () in
  let form =
    Form_field.make
      ~label:"Включить анализ T2-MI"
      ~align_end:true
      enabled in
  form, event

let make_pid () =
  let event, push = React.E.create () in
  let pid =
    Textfield.make_textfield
      ~required:true
      ~label:"T2-MI PID"
      (Integer (Some 0, Some 8192)) in
  let listener =
    Events.inputs pid#input_element (fun _ _ ->
        push ();
        Lwt.return_unit) in
  pid#set_on_destroy (fun () -> Lwt.cancel listener);
  pid, event

let make_sid () =
  let event, push = React.E.create () in
  let sid =
    Textfield.make_textfield
      ~required:true
      ~label:"T2-MI Stream ID"
      (Integer (Some 0, Some 7)) in
  let listener =
    Events.inputs sid#input_element (fun _ _ ->
        push ();
        Lwt.return_unit) in
  sid#set_on_destroy (fun () -> Lwt.cancel listener);
  sid, event

let rec stream_to_string (s : Stream.t) =
  let src = match s.source.node with
    | Entry Input i -> "Вход " ^ Topology.get_input_name i
    | Entry Board b -> "Плата " ^ Topology.get_board_name b
    | Stream s -> "Поток,  " ^ stream_to_string s in
  match Stream.Source.to_string s.source.info with
  | "" -> src
  | s -> Printf.sprintf "%s. %s" src s

let stream_select_validation = Select.(
    { to_string = Yojson.Safe.to_string % Stream.to_yojson
    ; of_string = Stream.of_yojson % Yojson.Safe.from_string
    })

let make_stream_select_items streams =
  List.map (fun (s : Stream.t) ->
      let open Select.Markup.Native in
      create_option
        ~value:(stream_select_validation.to_string s)
        ~text:(stream_to_string s)
        ()) streams

let make_stream_select
    (streams : Stream.t list)
    (_ : t2mi_mode) =
  let event, push = React.E.create () in
  (* let streams =
   *   React.S.l2
   *     ~eq:(Util_equal.List.equal Stream.equal)
   *     (fun (set : t2mi_mode) lst ->
   *        match set.stream with
   *        | ID _ -> lst
   *        | Full s ->
   *          let streams = Utils.List.add_nodup ~eq:Stream.equal s lst in
   *          List.sort Stream.compare streams)
   *     mode streams in *)
  let select =
    Select.make_native
      ~on_change:(fun _ -> push ())
      ~label:"Поток для анализа T2-MI"
      ~items:(make_stream_select_items streams)
      (Custom stream_select_validation) in
  let set (x : t2mi_mode) =
    match x.stream with
    | ID _ -> ()
    | Full s -> select#set_value s in
  select, set, event

let name = "Настройки. T2-MI"
let settings = None

type event =
  [ `Mode of t2mi_mode
  | `State of Topology.state
  | `Incoming_streams of Stream.t list
  ]

class t
    (state : Topology.state)
    (mode : t2mi_mode)
    (streams : Stream.t list)
    (control : int) =
  let en, e_en = make_enabled () in
  let pid, e_pid = make_pid () in
  let sid, e_sid = make_sid () in
  let stream_select, _, e_stream = make_stream_select streams mode in
  let submit = Button.make ~label:"Применить" () in
  let buttons = Card.Actions.make_buttons [submit] in
  let actions = Card.Actions.make [buttons] in
  object(self)
    val mutable _on_submit = None
    val mutable _e_change = None
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#append_child en;
      super#append_child stream_select;
      super#append_child pid;
      super#append_child sid;
      super#append_child actions;
      super#add_class base_class;
      super#add_class Box.CSS.root;
      super#add_class Box.CSS.vertical;
      self#set_value mode;
      self#notify (`State state);
      _e_change <- Some (React.E.map (fun () ->
          match self#value with
          | None -> submit#set_disabled true
          | Some _ -> submit#set_disabled false)
         @@ React.E.select [e_en; e_pid; e_sid; e_stream]);
      _on_submit <- Some (Events.clicks submit#root (fun _ _ ->
          Lwt.map (fun _ -> ()) @@ self#submit ()))

    method! destroy () : unit =
      super#destroy ();
      Utils.Option.iter (React.E.stop ~strong:true) _e_change;
      Utils.Option.iter Lwt.cancel _on_submit;
      _e_change <- None;
      _on_submit <- None

    method submit () : (unit, string) Lwt_result.t =
      match self#value with
      | None -> Lwt.return_error "Please fill the settings form"
      | Some mode ->
        Lwt_result.map_err Api_js.Http.error_to_string
        @@ Http_device.set_t2mi_mode mode control

    method value : t2mi_mode option =
      match pid#value, sid#value, stream_select#value with
      | Some pid, Some t2mi_stream_id, Some stream ->
        Some { enabled = en#input#checked
             ; pid
             ; t2mi_stream_id
             ; stream = Full stream
             }
      | _ -> None

    method set_value (mode : t2mi_mode) : unit =
      en#input#toggle ~force:mode.enabled ();
      pid#set_value mode.pid;
      sid#set_value mode.t2mi_stream_id

    method notify : event -> unit = function
      | `Mode mode -> self#set_value mode
      | `Incoming_streams streams ->
        let value = stream_select#value in
        let items = make_stream_select_items streams in
        stream_select#remove_children ();
        List.iter (Element.append_child stream_select#root)
        @@ List.map Tyxml_js.To_dom.of_option items;
        Utils.Option.iter stream_select#set_value value
      | `State s ->
        let disabled = match s with `Fine -> false | _ -> true in
        en#input#set_disabled disabled;
        stream_select#set_disabled disabled;
        pid#set_disabled disabled;
        sid#set_disabled disabled
  end

let make (state : Topology.state)
    (mode : t2mi_mode)
    (streams : Stream.t list)
    (control : int) =
  new t state mode streams control
