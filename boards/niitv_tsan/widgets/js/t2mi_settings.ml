open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_http_js
open Components

let base_class = "qos-niit-t2mi-settings"

let make_enabled () =
  let event, push = React.E.create () in
  let enabled =
    Switch.make
      ~on_change:(fun _ ->
        push ();
        Lwt.return_unit)
      ()
  in
  let form =
    Form_field.make_of_widget
      ~label:(`Text "Включить анализ T2-MI")
      ~align_end:true
      ~input:enabled
      ()
  in
  form, event

let make_pid () =
  let event, push = React.E.create () in
  let pid =
    Textfield.make
      ~required:true
      ~label:(`Text "T2-MI PID")
      ~validation:(Integer (Some 0, Some 8192))
      ()
  in
  let listener =
    Js_of_ocaml_lwt.Lwt_js_events.inputs pid#input_element (fun _ _ ->
        push ();
        Lwt.return_unit)
  in
  pid#set_on_destroy (fun () -> Lwt.cancel listener);
  pid, event

let make_sid () =
  let event, push = React.E.create () in
  let sid =
    Textfield.make
      ~required:true
      ~label:(`Text "T2-MI Stream ID")
      ~validation:(Integer (Some 0, Some 7))
      ()
  in
  let listener =
    Js_of_ocaml_lwt.Lwt_js_events.inputs sid#input_element (fun _ _ ->
        push ();
        Lwt.return_unit)
  in
  sid#set_on_destroy (fun () -> Lwt.cancel listener);
  sid, event

let rec stream_to_string (s : Stream.t) =
  let src =
    match s.source.node with
    | Entry (Input i) -> "Вход " ^ Topology.get_input_name i
    | Entry (Board b) -> "Плата " ^ Topology.get_board_name b
    | Stream s -> "Поток,  " ^ stream_to_string s
  in
  match Stream.Source.to_string s.source.info with
  | "" -> src
  | s -> Printf.sprintf "%s. %s" src s

let stream_select_validation =
  Select.
    { to_string = Yojson.Safe.to_string % Stream.to_yojson
    ; of_string =
        (fun json ->
          try Stream.of_yojson @@ Yojson.Safe.from_string json
          with Yojson.Json_error s -> Error s) }

let make_stream_select_items streams =
  List.map
    (fun (s : Stream.t) ->
      let open Select.Markup_js.Native in
      create_option
        ~value:(stream_select_validation.to_string s)
        ~text:(stream_to_string s)
        ())
    streams

let make_stream_select (streams : Stream.t list) (mode : t2mi_mode) =
  let event, push = React.E.create () in
  let streams =
    match mode.stream with
    | ID _ -> streams
    | Full s ->
        if List.exists (fun x -> Stream.equal x s) streams then streams else s :: streams
  in
  let streams = List.sort Stream.compare streams in
  let validation = Select.(Custom stream_select_validation) in
  let options =
    Select.native_options_of_values ~label:stream_to_string validation streams
  in
  let select =
    Select.make_native
      ~on_change:(fun _ -> push ())
      ~label:(`Text "Поток для анализа T2-MI")
      ~options
      ~validation
      ()
  in
  select, event

let name = "Настройки. T2-MI"

let settings = None

type event =
  [ `Mode of t2mi_mode
  | `State of Topology.state
  | `Incoming_streams of Stream.t list ]

class t
  (state : Topology.state)
  (mode : t2mi_mode)
  (streams : Stream.t list)
  (control : int) =
  let en, e_en = make_enabled () in
  let pid, e_pid = make_pid () in
  let sid, e_sid = make_sid () in
  let stream_select, e_stream = make_stream_select streams mode in
  let submit = Button.make ~label:"Применить" () in
  let buttons = Card.Markup_js.create_action_buttons ~children:[submit#markup] () in
  let actions = Card.Markup_js.create_actions ~children:[buttons] () in
  object (self)
    val mutable _on_submit = None

    val mutable _e_change = None

    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#append_child en;
      super#append_child stream_select;
      super#append_child pid;
      super#append_child sid;
      Dom.appendChild super#root @@ Tyxml_js.To_dom.of_element actions;
      super#add_class base_class;
      super#add_class Box.CSS.root;
      super#add_class Box.CSS.vertical;
      self#set_value mode;
      self#notify (`State state);
      _e_change <-
        Some
          (React.E.map self#update_submit_button_state
          @@ React.E.select [e_en; e_pid; e_sid; e_stream]);
      _on_submit <-
        Some
          (Js_of_ocaml_lwt.Lwt_js_events.clicks submit#root (fun _ _ ->
               Lwt.map (fun _ -> ()) @@ self#submit ()))

    method! destroy () : unit =
      super#destroy ();
      Option.iter (React.E.stop ~strong:true) _e_change;
      Option.iter Lwt.cancel _on_submit;
      _e_change <- None;
      _on_submit <- None

    method submit () : (unit, [`Msg of string]) Lwt_result.t =
      match self#value with
      | None -> Lwt.return_error (`Msg "Please fill the settings form")
      | Some mode ->
          let t = Http_device.set_t2mi_mode mode control in
          submit#set_loading_lwt t;
          t

    method value : t2mi_mode option =
      let disabled =
        (en#input)#disabled || pid#disabled || sid#disabled || stream_select#disabled
      in
      match disabled, pid#value, sid#value, stream_select#value with
      | false, Some pid, Some t2mi_stream_id, Some stream ->
          Some {enabled = (en#input)#checked; pid; t2mi_stream_id; stream = Full stream}
      | _ -> None

    method set_value (mode : t2mi_mode) : unit =
      (en#input)#toggle ~force:mode.enabled ();
      pid#set_value mode.pid;
      sid#set_value mode.t2mi_stream_id;
      (match mode.stream with
      | ID _ -> ()
      | Full s -> stream_select#set_value s);
      self#update_submit_button_state ()

    method notify : event -> unit =
      function
      | `Mode mode -> self#set_value mode
      | `Incoming_streams streams ->
          let value = stream_select#value in
          let streams =
            match value with
            | None -> streams
            | Some s ->
                if List.exists (Stream.equal s) streams then streams else s :: streams
          in
          let streams = List.sort Stream.compare streams in
          let items = make_stream_select_items streams in
          stream_select#clear ();
          List.iter stream_select#append_item @@ List.map Tyxml_js.To_dom.of_option items;
          stream_select#layout ();
          Option.iter stream_select#set_value value;
          self#update_submit_button_state ()
      | `State s ->
          let disabled =
            match s with
            | `Fine -> false
            | _ -> true
          in
          (en#input)#set_disabled disabled;
          stream_select#set_disabled disabled;
          pid#set_disabled disabled;
          sid#set_disabled disabled;
          self#update_submit_button_state ()

    method private update_submit_button_state () =
      match self#value with
      | Some _ -> submit#set_disabled false
      | None -> submit#set_disabled true
  end

let make
    (state : Topology.state)
    (mode : t2mi_mode)
    (streams : Stream.t list)
    (control : int) =
  new t state mode streams control
