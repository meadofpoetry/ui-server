open Board_types
open Containers
open Components

let make_t2mi_enabled (init: t2mi_mode option) =
  let enabled = new Switch.t () in
  let form    = new Form_field.t ~input:enabled ~label:"Включить" ~align_end:true () in
  let set x   = enabled#set_checked (Option.get_or ~default:false (Option.map (fun x -> x.enabled) x)) in
  set init;
  form#widget,set,enabled#s_state,enabled#set_disabled

let make_t2mi_pid (init: t2mi_mode option) =
  let pid = new Textfield.t
                ~input_id:"t2mi_pid_field"
                ~help_text:{validation=true;persistent=false;text=None}
                ~input_type:(Integer ((Some 0),(Some 8192)))
                ~label:"T2-MI PID"
                ()
  in
  let set x = match x with
    | Some (x:t2mi_mode) -> pid#fill_in x.pid
    | None               -> pid#clear
  in
  pid#set_required true;
  set init;
  pid#widget,set,pid#s_input,pid#set_disabled

let make_t2mi_sid (init: t2mi_mode option) =
  let sid = new Textfield.t
                ~input_id:"sid_field"
                ~help_text:{validation=true;persistent=false;text=None}
                ~input_type:(Integer (Some 0, Some 7))
                ~label:"T2-MI Stream ID"
                ()
  in
  let set x = match x with
    | Some (x:t2mi_mode) -> sid#fill_in x.t2mi_stream_id
    | None               -> sid#clear
  in
  sid#set_required true;
  set init;
  sid#widget,set,sid#s_input,sid#set_disabled

let make_stream_select (streams:  Common.Stream.t list React.signal) =
  let make_items sms =
    List.map (fun s -> new Select.Item.t ~value:s ~text:(Common.Stream.to_short_name s) ()) sms
  in
  let select = new Select.t ~default_selected:false ~label:"Потоки" ~items:[] () in
  let _      = React.S.map (fun sms -> let items = make_items sms in
                                       select#set_empty;
                                       List.iter (fun i -> select#append_item i) items) streams
  in
  select#widget

let make_t2mi_mode ~(init:    t2mi_mode option)
                   ~(event:   t2mi_mode option React.event)
                   ~(streams: Common.Stream.t list React.signal)
                   ~(state:   Common.Topology.state React.signal)
                   control
                   () : (t2mi_mode_request,unit) Ui_templates.Types.settings_block =
  let en,set_en,s_en,dis_en     = make_t2mi_enabled init in
  let pid,set_pid,s_pid,dis_pid = make_t2mi_pid init in
  let sid,set_sid,s_sid,dis_sid = make_t2mi_sid init in
  let ss = make_stream_select streams in
  let s : t2mi_mode_request option React.signal =
    React.S.l4 (fun en pid sid state->
        match en,pid,sid,state with
        | true,Some pid,Some sid,`Fine -> Some (Some { enabled = en
                                                     ; pid
                                                     ; t2mi_stream_id = sid
                                                     ; stream = Common.Stream.Single })
        | false,_,_,`Fine              -> Some None
        | _                            -> None)
               s_en s_pid s_sid state
  in
  let _   = React.S.l2 (fun state en -> let is_disabled = match state with
                                          | `Fine -> false
                                          | _     -> true
                                        in
                                        dis_en is_disabled;
                                        List.iter (fun f -> f (if is_disabled then true else not en))
                                                  [dis_pid; dis_sid])
                       state s_en
  in
  let _      = React.E.map (fun config -> List.iter (fun f -> f config) [set_en; set_pid; set_sid]) event in
  let submit = Requests.post_t2mi_mode control in
  let box    = new Box.t ~vertical:true ~widgets:[en;ss;pid;sid] () in
  box#widget,s,submit

let make_jitter_enabled (init:jitter_mode option) =
  let enabled = new Switch.t () in
  let form    = new Form_field.t ~input:enabled ~label:"Включить" ~align_end:true () in
  let set x   = enabled#set_checked @@ Option.is_some x in
  set init;
  form#widget,set,enabled#s_state,enabled#set_disabled

let make_jitter_pid (init:jitter_mode option) =
  let pid = new Textfield.t
                ~input_id:"jitter_pid_field"
                ~help_text:{validation=true;persistent=false;text=None}
                ~input_type:(Integer ((Some 0),(Some 8192)))
                ~label:"PID"
                ()
  in
  let set x = match x with
    | Some (x:jitter_mode) -> pid#fill_in x.pid
    | None                 -> pid#clear
  in
  pid#set_required true;
  set init;
  pid#widget,set,pid#s_input,pid#set_disabled

let make_jitter_mode ~(init:  jitter_mode option)
                     ~(event: jitter_mode option React.event)
                     ~(state: Common.Topology.state React.signal)
                     control
                     () : (jitter_mode_request,unit) Ui_templates.Types.settings_block =
  let en,set_en,s_en,dis_en     = make_jitter_enabled init in
  let pid,set_pid,s_pid,dis_pid = make_jitter_pid init in
  let _ = React.S.l2 (fun state en -> let is_disabled = match state with
                                        | `Fine -> false
                                        | _     -> true
                                      in
                                      dis_en is_disabled;
                                      List.iter (fun f -> f (if is_disabled then true else not en)) [dis_pid])
                     state s_en
  in
  let _ = React.E.map (fun config -> List.iter (fun f -> f config) [set_en; set_pid]) event in
  let s : jitter_mode_request option React.signal =
    React.S.l3 (fun en pid state ->
        match en,pid,state with
        | true,Some pid,`Fine -> Some (Some { pid; stream = Common.Stream.Single })
        | false,_,`Fine       -> Some None
        | _                   -> None)
               s_en s_pid state
  in
  let box = new Box.t ~vertical:true ~widgets:[en;pid] () in
  let submit = Requests.post_jitter_mode control in
  box#widget,s,submit
