open Containers
open Components
open Board_types
open Lwt_result.Infix

type config =
  { id : int
  } [@@deriving yojson]

let (%>) = Fun.(%>)

let base_class = "dvb-niit-module-settings"

let make_standard (init: standard) =
  let items = [ `Item (new Select.Item.t ~text:"DVB-T2" ~value:T2 ())
              ; `Item (new Select.Item.t ~text:"DVB-T"  ~value:T  ())
              ; `Item (new Select.Item.t ~text:"DVB-C"  ~value:C  ())
              ]
  in
  let mode  = new Select.t ~label:"Стандарт" ~items () in
  let set x = mode#set_selected_value ~eq:equal_standard x |> ignore in
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

let make_freq (standard : standard)
              (init     : int) =
  let items = List.map (fun (c:Channel.t) -> `Item (new Select.Item.t ~text:c.name ~value:c.freq ()))
                       (match standard with
                        | T2 | T -> Channel.Terrestrial.lst
                        | C      -> Channel.Cable.lst)
  in
  let freq  = new Select.t ~label:"ТВ канал" ~items () in
  let set x = freq#set_selected_value ~eq:(=) x |> ignore in
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

let make_mode_box ~(standard : standard)
                  ~(init     : channel)
                  ~(event    : channel React.event)
                  ~(state    : Common.Topology.state React.signal)
                  () =
  let freq,set_freq = make_freq standard init.freq in
  let bw,set_bw     = make_bw init.bw in
  let plp,set_plp   = make_plp init.plp in
  let b = new Box.t ~widgets:(match standard with
                              | T | C -> [ freq#widget; bw#widget ]
                              | T2    -> [ freq#widget; bw#widget; plp#widget ])
              () in
  let s = React.S.l3 (fun freq bw plp -> match freq,bw,plp with
                                         | Some freq,Some bw,Some plp -> Some { freq; bw; plp }
                                         | _                          -> None)
                     freq#s_selected_value bw#s_selected_value plp#s_input
  in
  let _ = React.E.map (fun (s:channel) -> set_freq s.freq; set_bw s.bw; set_plp s.plp) event in
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
      () =
  let standard,set_standard = make_standard init.standard in
  let t2_box,s_t2 =
    make_mode_box ~standard:T2 ~state ~init:init.t2 ~event:(React.E.map (fun x -> x.t2) event) () in
  let t_box,s_t   =
    make_mode_box ~standard:T  ~state ~init:init.t  ~event:(React.E.map (fun x -> x.t)  event) () in
  let c_box,s_c   =
    make_mode_box ~standard:C  ~state ~init:init.c  ~event:(React.E.map (fun x -> x.c)  event) () in
  let s : (int * mode) option React.signal =
    React.S.l5 (fun standard t2 t c state ->
        match standard,t2,t,c,state with
        | Some T2,Some t2,_,_,`Fine -> Some (id,{ standard = T2; channel = t2 })
        | Some T,_,Some t,_,`Fine   -> Some (id,{ standard = T ; channel = t  })
        | Some C,_,_,Some c,`Fine   -> Some (id,{ standard = C ; channel = c  })
        | _                         -> None)
      standard#s_selected_value s_t2 s_t s_c state
  in
  let box = new Box.t ~widgets:[ standard#widget; t2_box#widget; t_box#widget; c_box#widget ] () in
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
  let _ = React.S.map update_visibility standard#s_selected_value in
  let _ = React.E.map (fun c -> set_standard c.standard) event in
  let _ = React.S.map (function
              | `No_response | `Init -> standard#set_disabled true
              | `Fine                -> standard#set_disabled false) state
  in
  let () = box#add_class base_class in
  let submit = fun (id,m) -> Requests.Receiver.HTTP.post_mode ~id m control >|= (fun _ -> ()) in
  box#widget,s,submit

let default_config = { id = 0 }

let name conf = Printf.sprintf "Модуль %d. Настройки" (succ (Option.get_or ~default:default_config conf).id)
let settings  = None

let make ~(state:  Common.Topology.state React.signal)
         ~(config: Board_types.config React.signal)
         (conf:    config option)
         control =
  let conf = Option.get_or ~default:default_config conf in
  let get = List.Assoc.get_exn ~eq:(=) conf.id in
  let w,s,set = make_module_settings ~id:conf.id
                                     ~init:(get @@ React.S.value config)
                                     ~event:(React.S.changes @@ React.S.map get config)
                                     ~state
                                     control
                                     ()
  in
  let a   = Ui_templates.Buttons.create_apply s set in
  let box = new Box.t ~vertical:true ~widgets:[w;a#widget] () in
  let ()  = box#add_class "mdc-settings-widget" in
  box#widget
