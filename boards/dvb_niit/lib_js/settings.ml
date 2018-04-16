open Containers
open Board_types
open Components

let (%>) = Fun.(%>)

let base_class = "dvb-niit-module-settings"

let make_mode (init: mode) =
  let items = [ `Item (new Select.Item.t ~text:"DVB-T2" ~value:T2 ())
              ; `Item (new Select.Item.t ~text:"DVB-T"  ~value:T  ())
              ; `Item (new Select.Item.t ~text:"DVB-C"  ~value:C  ())
              ]
  in
  let mode  = new Select.t ~label:"Стандарт" ~items () in
  let set x = mode#set_selected_value ~eq:equal_mode x |> ignore in
  set init;
  mode#add_class @@ Markup.CSS.add_element base_class "mode";
  mode,set

let make_bw (init: bw) =
  let items = [ `Item (new Select.Item.t ~text:"6 МГц" ~value:Bw6 ())
              ; `Item (new Select.Item.t ~text:"7 МГц" ~value:Bw7 ())
              ; `Item (new Select.Item.t ~text:"8 МГц" ~value:Bw8 ())
              ]
  in
  let bw    = new Select.t ~label:"Полоса пропускания" ~items () in
  let set x = bw#set_selected_value ~eq:equal_bw x |> ignore in
  set init;
  bw,set

let make_freq (mode: mode)
              (init: int32) =
  let items = List.map (fun (c:Channel.t) -> `Item (new Select.Item.t ~text:c.name ~value:c.freq ()))
                       (match mode with
                        | T2 | T -> Channel.Terrestrial.lst
                        | C      -> Channel.Cable.lst)
  in
  let freq  = new Select.t ~label:"ТВ канал" ~items () in
  let set x = freq#set_selected_value ~eq:(=) (Int32.to_int x) |> ignore in
  set init;
  freq,set

let make_plp (init: int) =
  let ht : Textfield.Help_text.helptext = {persistent=false;validation=true;text=None} in
  let plp = new Textfield.t
                ~input_id:"plp_field"
                ~input_type:(Integer ((Some 0),(Some 255)))
                ~help_text:ht
                ~label:"PLP ID"
                ()
  in
  let ()  = plp#set_required true in
  let set = plp#fill_in in
  set init;
  plp,set

let make_mode_box ~(mode  : mode)
                  ~(init  : channel_settings)
                  ~(event : channel_settings React.event)
                  ~(state : Common.Topology.state React.signal)
                  () =
  let freq,set_freq = make_freq mode init.freq in
  let bw,set_bw     = make_bw init.bw in
  let plp,set_plp   = make_plp init.plp in
  let b = new Box.t ~widgets:(match mode with
                              | T | C -> [ freq#widget; bw#widget ]
                              | T2    -> [ freq#widget; bw#widget; plp#widget ])
              () in
  let s = React.S.l3 (fun freq bw plp -> match freq,bw,plp with
                                         | Some freq,Some bw,Some plp -> Some { freq = Int32.of_int freq; bw; plp }
                                         | _                          -> None)
                     freq#s_selected_value bw#s_selected_value plp#s_input
  in
  let _ = React.E.map (fun (s:channel_settings) -> set_freq s.freq; set_bw s.bw; set_plp s.plp) event in
  let _ = React.S.map (fun x -> let is_disabled = match x with
                                  | `Fine -> false
                                  | _     -> true
                                in
                                freq#set_disabled is_disabled;
                                bw#set_disabled is_disabled;
                                plp#set_disabled is_disabled)
                      state
  in
  b,s

let make_module_settings ~(id:    int)
                         ~(init:  config_item)
                         ~(event: config_item React.event)
                         ~(state: Common.Topology.state React.signal)
                         control
                         () : (settings_request,settings_response) Ui_templates.Types.settings_block =
  let mode,set_mode = make_mode init.mode in
  let t2_box,s_t2 = make_mode_box ~mode:T2 ~state ~init:init.t2 ~event:(React.E.map (fun x -> x.t2) event) () in
  let t_box,s_t   = make_mode_box ~mode:T  ~state ~init:init.t  ~event:(React.E.map (fun x -> x.t)  event) () in
  let c_box,s_c   = make_mode_box ~mode:C  ~state ~init:init.c  ~event:(React.E.map (fun x -> x.c)  event) () in
  let s : settings_request option React.signal =
    React.S.l5 (fun mode t2 t c state ->
        match mode,t2,t,c,state with
        | Some T2,Some t2,_,_,`Fine -> Some (id,{ mode = T2; channel = t2})
        | Some T,_,Some t,_,`Fine   -> Some (id,{ mode = T ; channel = t })
        | Some C,_,_,Some c,`Fine   -> Some (id,{ mode = C ; channel = c })
        | _                         -> None)
               mode#s_selected_value s_t2 s_t s_c state
  in
  let box = new Box.t ~widgets:[ mode#widget; t2_box#widget; t_box#widget; c_box#widget ] () in
  let update_visibility = function
    | Some T2 -> (try Dom.removeChild box#root t_box#root with _ -> ());
                 (try Dom.removeChild box#root c_box#root with _ -> ());
                 Dom.appendChild box#root t2_box#root
    | Some T  -> (try Dom.removeChild box#root t2_box#root with _ -> ());
                 (try Dom.removeChild box#root c_box#root with _ -> ());
                 Dom.appendChild box#root t_box#root
    | Some C  -> (try Dom.removeChild box#root t2_box#root with _ -> ());
                 (try Dom.removeChild box#root t_box#root with _ -> ());
                 Dom.appendChild box#root c_box#root
    | None    -> (try Dom.removeChild box#root t2_box#root with _ -> ());
                 (try Dom.removeChild box#root t_box#root with _ -> ());
                 (try Dom.removeChild box#root c_box#root with _ -> ())
  in
  let _ = React.S.map update_visibility mode#s_selected_value in
  let _ = React.E.map (fun c -> set_mode c.mode) event in
  let _ = React.S.map (function
                       | `No_response | `Init -> mode#set_disabled true
                       | `Fine                -> mode#set_disabled false) state
  in
  let () = box#add_class base_class in
  let submit = fun (cfg:settings_request) -> Requests.post_settings control cfg in
  box#widget,s,submit
