open Board_types
open Containers
open Components
open Common

type config = unit [@@deriving yojson]

let base_class = "qos-niit-t2mi-settings"

let make_enabled () =
  let enabled = new Switch.t () in
  let form =
    new Form_field.t
      ~input:enabled
      ~label:"Включить анализ T2-MI"
      ~align_end:true
      () in
  let set x =
    let v = Option.(get_or ~default:false (map (fun x -> x.enabled) x)) in
    enabled#set_checked v in
  form#widget, set, enabled#s_state, enabled#set_disabled

let make_pid () =
  let pid =
    new Textfield.t
      (* ~help_text:{ validation = true
       *            ; persistent = false
       *            ; text = None } *)
      ~input_type:(Integer (Some 0, Some 8192))
      ~label:"T2-MI PID"
      () in
  let set x = match x with
    | Some (x : t2mi_mode) -> pid#set_value x.pid
    | None -> pid#clear () in
  pid#set_required true;
  pid#widget, set, pid#s_input, pid#set_disabled

let make_sid () =
  let sid =
    new Textfield.t
      ~input_type:(Integer (Some 0, Some 7))
      ~label:"T2-MI Stream ID"
      () in
  let set x = match x with
    | Some (x : t2mi_mode) -> sid#set_value x.t2mi_stream_id
    | None -> sid#clear () in
  sid#set_required true;
  sid#widget, set, sid#s_input, sid#set_disabled

let make_stream_select (streams : Stream.t list React.signal)
      (mode : t2mi_mode option React.signal) =
  let streams =
    React.S.l2
      ~eq:(Equal.list Stream.equal)
      (fun (set : t2mi_mode option) lst ->
        let streams = match set, lst with
        | Some s, lst -> List.add_nodup ~eq:Stream.equal s.stream lst
        | None, lst -> lst in
        List.sort Stream.compare streams)
      mode streams in
  let make_items sms =
    List.map (fun (s : Stream.t) ->
        new Select.Item.t
          ~value:s
          ~text:(Stream.Source.to_string s.source.info) (* FIXME make normal name *)
          ()) sms in
  let select =
    new Select.t
      ~default_selected:false
      ~label:"Поток для анализа T2-MI"
      ~items:[]
      () in
  let _s =
    React.S.map ~eq:Equal.unit
      (fun sms ->
        let eq = Stream.equal in
        let value = select#value in
        let items = make_items sms in
        select#set_empty ();
        List.iter select#append_item items;
        Option.iter Fun.(ignore % select#set_selected_value ~eq) value)
      streams in
  let set x = match x with
    | Some (x : t2mi_mode) ->
       ignore @@ select#set_selected_value ~eq:Stream.equal x.stream
    | None -> select#set_selected_index 0 in
  select#set_on_destroy (fun () -> React.S.stop ~strong:true _s);
  select#widget,
  set,
  select#s_selected_value,
  select#set_disabled

let name = "Настройки. T2-MI"
let settings = None

let make ~(state : Topology.state React.signal)
      ~(mode : t2mi_mode option React.signal)
      ~(streams : Stream.t list React.signal)
      (_ : config option)
      (control : int) =
  let en, set_en, s_en, dis_en = make_enabled () in
  let pid, set_pid, s_pid, dis_pid = make_pid () in
  let sid, set_sid, s_sid, dis_sid = make_sid () in
  let ss, set_stream, s_stream, dis_stream =
    make_stream_select streams mode in
  let (s : t2mi_mode option option React.signal) =
    React.S.l5 ~eq:(Equal.option (Equal.option equal_t2mi_mode))
      (fun en pid sid stream state ->
        match en, pid, sid, stream, state with
        | en, Some pid, Some sid, Some stream, `Fine ->
           Some (Some { enabled = en
                      ; pid
                      ; t2mi_stream_id = sid
                      ; stream }) (* FIXME stream *)
        | false, _, _, _, `Fine -> Some None
        | _ -> None)
      s_en s_pid s_sid s_stream state in
  let s_dis =
    React.S.l2 ~eq:Equal.unit (fun state en ->
        let is_disabled = match state with
          | `Fine -> false
          | _ -> true in
        dis_en is_disabled;
        List.iter (fun f -> f (if is_disabled then true else not en))
          [dis_pid; dis_sid; dis_stream]) state s_en in
  let s_set =
    React.S.map ~eq:Equal.unit (fun x ->
        let setters = [set_en; set_pid; set_sid; set_stream] in
        List.iter (fun f -> f x) setters) mode in
  let submit = fun x -> Requests.Device.HTTP.post_t2mi_mode x control in
  let apply = new Ui_templates.Buttons.Set.t s submit () in
  let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
  let actions = new Card.Actions.t ~widgets:[buttons] () in
  let box = new Vbox.t ~widgets:[en; ss; pid; sid; actions#widget] () in
  box#add_class base_class;
  box#set_on_destroy (fun () ->
      React.S.stop ~strong:true s_dis;
      React.S.stop ~strong:true s_set);
  box#widget
