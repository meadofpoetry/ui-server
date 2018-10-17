open Board_types
open Containers
open Components
open Common

type config = unit [@@deriving yojson]

let base_class = "qos-niit-jitter-settings"

let make_enabled () =
  let enabled = new Switch.t () in
  let form    =
    new Form_field.t
      ~input:enabled
      ~label:"Включить"
      ~align_end:true () in
  let set x   = enabled#set_checked @@ Option.is_some x in
  form#widget,set,enabled#s_state,enabled#set_disabled

let make_pid () =
  let pid =
    new Textfield.t
      (* ~help_text:{validation=true;persistent=false;text=None} *)
      ~input_type:(Integer ((Some 0),(Some 8192)))
      ~label:"PID"
      () in
  let set x = match x with
    | Some (x:jitter_mode) -> pid#set_value x.pid
    | None                 -> pid#clear () in
  pid#set_required true;
  pid#widget,set,pid#s_input,pid#set_disabled

let name     = "Настройки. Джиттер"
let settings = None

let make ~(state: Topology.state React.signal)
         ~(mode:  jitter_mode option React.signal)
         (conf:   config option)
         control : Widget.t =
  let en,set_en,s_en,dis_en     = make_enabled () in
  let pid,set_pid,s_pid,dis_pid = make_pid () in
  let _ =
    React.S.l2 (fun state en ->
        let is_disabled = match state with
          | `Fine -> false
          | _     -> true in
        dis_en is_disabled;
        List.iter (fun f -> f (if is_disabled then true else not en))
          [dis_pid])
      state s_en
  in
  let _ =
    React.S.map (fun x -> List.iter (fun f -> f x) [set_en; set_pid]) mode in
  let s : jitter_mode option option React.signal =
    React.S.l3 ~eq:(Equal.option (Equal.option equal_jitter_mode))
      (fun en pid state ->
        match en, pid, state with
        | true, Some pid, `Fine ->
           Some (Some { pid
                      ; stream = Stream.Multi_TS_ID.of_int32_raw 0l }) (* FIXME stream *)
        | false, _, `Fine -> Some None
        | _ -> None)
      s_en s_pid state in
  let submit = fun x -> Requests.Device.HTTP.post_jitter_mode x control in
  let apply = new Ui_templates.Buttons.Set.t s submit () in
  let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
  let actions = new Card.Actions.t ~widgets:[buttons] () in
  let box = new Vbox.t ~widgets:[en; pid; actions#widget] () in
  box#add_class base_class;
  box#widget
