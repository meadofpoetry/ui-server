open Board_types
open Containers
open Components
open Lwt_result.Infix

type config = unit [@@deriving yojson]

let make_enabled () =
  let enabled = new Switch.t () in
  let form    = new Form_field.t ~input:enabled ~label:"Включить" ~align_end:true () in
  let set x   = enabled#set_checked (Option.get_or ~default:false (Option.map (fun x -> x.enabled) x)) in
  form#widget,set,enabled#s_state,enabled#set_disabled

let make_pid () =
  let pid = new Textfield.t
                ~input_id:"t2mi_pid_field"
                ~help_text:{validation=true;persistent=false;text=None}
                ~input_type:(Integer ((Some 0),(Some 8192)))
                ~label:"T2-MI PID"
                ()
  in
  let set x = match x with
    | Some (x:t2mi_mode) -> pid#fill_in x.pid
    | None               -> pid#clear ()
  in
  pid#set_required true;
  pid#widget,set,pid#s_input,pid#set_disabled

let make_sid () =
  let sid = new Textfield.t
                ~input_id:"sid_field"
                ~help_text:{validation=true;persistent=false;text=None}
                ~input_type:(Integer (Some 0, Some 7))
                ~label:"T2-MI Stream ID"
                ()
  in
  let set x = match x with
    | Some (x:t2mi_mode) -> sid#fill_in x.t2mi_stream_id
    | None               -> sid#clear ()
  in
  sid#set_required true;
  sid#widget,set,sid#s_input,sid#set_disabled

let make_stream_select (streams : Common.Stream.t list React.signal) =
  let make_items sms =
    List.map (fun s -> new Select.Item.t ~value:s ~text:(Common.Stream.to_short_name s) ()) sms
  in
  let select = new Select.t ~default_selected:false ~label:"Потоки" ~items:[] () in
  let _      = React.S.map (fun sms -> let items = make_items sms in
                                       select#set_empty ();
                                       List.iter (fun i -> select#append_item i) items) streams
  in
  select#widget

let name     = "Настройки. T2-MI"
let settings = None

let make ~(state:   Common.Topology.state React.signal)
         ~(mode:    t2mi_mode option React.signal)
         ~(streams: Common.Stream.t_list React.signal)
         (conf:     config option)
         control =
  let en,set_en,s_en,dis_en     = make_enabled () in
  let pid,set_pid,s_pid,dis_pid = make_pid () in
  let sid,set_sid,s_sid,dis_sid = make_sid () in
  let ss = make_stream_select streams in
  let s : t2mi_mode_opt option React.signal =
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
  let _      = React.S.map (fun x -> List.iter (fun f -> f x) [set_en; set_pid; set_sid]) mode in
  let submit = Requests.Device.HTTP.post_t2mi_mode control in
  let apply  = Ui_templates.Buttons.create_apply s submit in
  let box    = new Box.t ~vertical:true ~widgets:[en;ss;pid;sid;apply#widget] () in
  let ()     = box#add_class "mdc-settings-widget" in
  box#widget
