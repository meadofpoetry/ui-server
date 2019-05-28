open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_http_js
open Containers
open Components

let base_class = "qos-niit-t2mi-settings"

let make_enabled () =
  let signal, push = React.S.create false in
  let enabled = Switch.make ~on_change:(fun x ->
      push x#checked;
      Lwt.return_unit)
      () in
  let form =
    Form_field.make
      ~label:"Включить анализ T2-MI"
      ~align_end:true
      enabled in
  form, signal

let make_pid () =
  let signal, push = React.S.create None in
  let pid =
    Textfield.make_textfield
      ~required:true
      ~label:"T2-MI PID"
      (Integer (Some 0, Some 8192)) in
  let listener =
    Events.inputs pid#input_element (fun _ _ ->
        push pid#value;
        Lwt.return_unit) in
  pid#set_on_destroy (fun () -> Lwt.cancel listener);
  pid, signal

let make_sid () =
  let signal, push = React.S.create None in
  let sid =
    Textfield.make_textfield
      ~required:true
      ~label:"T2-MI Stream ID"
      (Integer (Some 0, Some 7)) in
  let listener =
    Events.inputs sid#input_element (fun _ _ ->
        push sid#value;
        Lwt.return_unit) in
  sid#set_on_destroy (fun () -> Lwt.cancel listener);
  sid, signal

let rec stream_to_string (s : Stream.t) =
  let src = match s.source.node with
    | Entry Input i -> "Вход " ^ Topology.get_input_name i
    | Entry Board b -> "Плата " ^ Topology.get_board_name b
    | Stream s -> "Поток,  " ^ stream_to_string s in
  match Stream.Source.to_string s.source.info with
  | "" -> src
  | s -> Printf.sprintf "%s. %s" src s

(* let make_stream_select
 *     (streams : Stream.t list React.signal)
 *     (mode : t2mi_mode React.signal) =
 *   let streams =
 *     React.S.l2
 *       ~eq:(Equal.list Stream.equal)
 *       (fun (set : t2mi_mode) lst ->
 *          match set.stream with
 *          | ID _ -> lst
 *          | Full s ->
 *            let streams = List.add_nodup ~eq:Stream.equal s lst in
 *            List.sort Stream.compare streams)
 *       mode streams in
 *   let make_items sms =
 *     List.map (fun (s : Stream.t) ->
 *         new Select.Item.t
 *           ~value:s
 *           ~text:(stream_to_string s)
 *           ()) sms in
 *   let select =
 *     new Select.t
 *       ~default_selected:false
 *       ~label:"Поток для анализа T2-MI"
 *       ~items:[]
 *       () in
 *   let _s =
 *     React.S.map ~eq:Equal.unit
 *       (fun sms ->
 *          let eq = Stream.equal in
 *          let value = select#value in
 *          let items = make_items sms in
 *          select#set_empty ();
 *          List.iter select#append_item items;
 *          Option.iter Fun.(ignore % select#set_selected_value ~eq) value)
 *       streams in
 *   let set (x : t2mi_mode) =
 *     match x.stream with
 *     | ID _ -> ()
 *     | Full s -> ignore @@ select#set_selected_value ~eq:Stream.equal s in
 *   select#set_on_destroy (fun () -> React.S.stop ~strong:true _s);
 *   select#widget,
 *   set,
 *   select#s_selected_value,
 *   select#set_disabled *)

let name = "Настройки. T2-MI"
let settings = None

type event =
  [ `Mode of t2mi_mode
  | `State of Topology.state
  ]

class t
    (state : Topology.state)
    (mode : t2mi_mode)
    (control : int)
    elt =
  let s_state, set_state = React.S.create state in
  let en, s_en = make_enabled () in
  let pid, s_pid = make_pid () in
  let sid, s_sid = make_sid () in
  let submit = Button.make ~label:"Применить" () in
  let buttons = Card.Actions.make_buttons [submit] in
  let actions = Card.Actions.make [buttons] in
  let s_stream = React.S.const (Some (ID Stream.Multi_TS_ID.forbidden)) in
  let (s : t2mi_mode option React.signal) =
    React.S.l5 ~eq:(Util_equal.Option.equal equal_t2mi_mode)
      (fun en pid sid stream state ->
         match en, pid, sid, stream, state with
         | en, Some pid, Some sid, Some stream, `Fine ->
           Some { enabled = en
                ; pid
                ; t2mi_stream_id = sid
                ; stream
                }
         | _ -> None)
      s_en s_pid s_sid s_stream s_state in
  object(self)
    val mutable _on_submit = None
    inherit Box.t ~widgets:[ en#widget
                           ; pid#widget
                           ; sid#widget
                           ; actions#widget ]
        elt () as super

    method! init () : unit =
      super#init ();
      super#add_class base_class;
      self#set_ mode;
      _on_submit <- Some (Events.clicks submit#root (fun _ _ ->
          Lwt.return_unit))

    method! destroy () : unit =
      super#destroy ();
      React.S.stop ~strong:true s;
      Option.iter Lwt.cancel _on_submit;
      _on_submit <- None

    method value : t2mi_mode option =
      match pid#value, sid#value with
      | Some pid, Some t2mi_stream_id ->
        Some { enabled = en#input#checked
             ; pid
             ; t2mi_stream_id
             ; stream = ID Stream.Multi_TS_ID.forbidden
             }
      | _ -> None

    method notify : event -> unit = function
      | `Mode mode -> self#set_ mode
      | `State s ->
        set_state s;
        let disabled = match s with `Fine -> false | _ -> true in
        en#input#set_disabled disabled;
        pid#set_disabled disabled;
        sid#set_disabled disabled

    method private set_ (mode : t2mi_mode) : unit =
      en#input#toggle ~force:mode.enabled ();
      pid#set_value mode.pid;
      sid#set_value mode.t2mi_stream_id
  end

let make (state : Topology.state)
    (mode : t2mi_mode)
    (control : int) =
  new t state mode control (assert false)
