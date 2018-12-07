open Containers
open Components
open Board_types.Device
open Lwt_result.Infix
open Common

type widget_config =
  { id : int
  } [@@deriving yojson]

let ( %> ) = Fun.( %> )

let base_class = "dvb-niit-module-settings"

let make_standard () =
  let items =
    [ `Item (new Select.Item.t ~text:"DVB-T2" ~value:T2 ())
    ; `Item (new Select.Item.t ~text:"DVB-T"  ~value:T  ())
    ; `Item (new Select.Item.t ~text:"DVB-C"  ~value:C  ())
    ] in
  let mode =
    new Select.t
      ~default_selected:false
      ~label:"Стандарт"
      ~items () in
  let set = function
    | None -> mode#set_selected_index 0
    | Some x ->
       mode#set_selected_value ~eq:equal_standard x.standard |> ignore in
  mode#add_class @@ Markup.CSS.add_element base_class "mode";
  mode, set

let make_bw (standard : standard option React.signal) () =
  let items =
    [ `Item (new Select.Item.t ~text:"6 МГц" ~value:Bw6 ())
    ; `Item (new Select.Item.t ~text:"7 МГц" ~value:Bw7 ())
    ; `Item (new Select.Item.t ~text:"8 МГц" ~value:Bw8 ())
    ] in
  let bw =
    new Select.t
      ~default_selected:false
      ~label:"Полоса пропускания"
      ~items () in
  let s_hide =
    React.S.map ~eq:Equal.unit (function
        | None -> bw#style##.display := Js_of_ocaml.Js.string "none"
        | Some _ -> bw#style##.display := Js_of_ocaml.Js.string "") standard in
  let set = function
    | None -> bw#set_selected_index 0
    | Some x -> bw#set_selected_value ~eq:equal_bw x.channel.bw |> ignore in
  bw, set, (fun () -> React.S.stop ~strong:true s_hide)

let make_freq ?(terrestrial = true)
      (standard : standard option React.signal)
      () =
  let items =
    List.map (fun (c : Channel.t) ->
        `Item (new Select.Item.t ~text:c.name ~value:c.freq ()))
      (if terrestrial then Channel.Terrestrial.lst
       else Channel.Cable.lst) in
  let freq =
    new Select.t
      ~default_selected:false
      ~label:"ТВ канал"
      ~items () in
  let s_hide =
    React.S.map ~eq:Equal.unit (fun x ->
        match x, terrestrial with
        | (Some T, true) | (Some T2, true) | (Some C, false) ->
           freq#style##.display := Js_of_ocaml.Js.string ""
        | _ -> freq#style##.display := Js_of_ocaml.Js.string "none") standard in
  let set = function
    | None -> freq#set_selected_index 0
    | Some x ->
       begin match x.standard, terrestrial with
       | (T, true) | (T2, true) | (C, false) ->
          freq#set_selected_value ~eq:(=) x.channel.freq |> ignore
       | _ -> ()
       end in
  freq, set, (fun () -> React.S.stop ~strong:true s_hide)

let make_plp (standard : standard option React.signal) () =
  let plp =
    new Textfield.t
      ~input_type:(Integer ((Some 0),(Some 255)))
      ~label:"PLP ID"
      () in
  plp#set_required true;
  let s_hide =
    React.S.map ~eq:Equal.unit (function
        | Some T2 -> plp#style##.display := Js_of_ocaml.Js.string ""
        | _ -> plp#style##.display := Js_of_ocaml.Js.string "none") standard in
  let set = function
    | None -> plp#clear ()
    | Some x -> plp#set_value x.channel.plp in
  plp, set, (fun () -> React.S.stop ~strong:true s_hide)

let make_mode_box ~(id : int)
      ~(init : mode option)
      ~(event : mode option React.event)
      ~(state : Common.Topology.state React.signal)
      control
      () =
  let std, set_std = make_standard () in
  let t_freq, set_t_freq, t_freq_close =
    make_freq ~terrestrial:true std#s_selected_value () in
  let c_freq, set_c_freq, c_freq_close =
    make_freq ~terrestrial:false std#s_selected_value () in
  let bw, set_bw, bw_close =
    make_bw std#s_selected_value () in
  let plp, set_plp, plp_close =
    make_plp std#s_selected_value () in
  let box =
    new Vbox.t
      ~widgets:[ std#widget
               ; t_freq#widget
               ; c_freq#widget
               ; bw#widget
               ; plp#widget ]
      () in
  let s =
    React.S.l6 ~eq:(Equal.option @@ Equal.pair Int.equal equal_mode)
      (fun standard t_freq c_freq bw plp state ->
        match standard, t_freq, c_freq, bw, plp, state with
        | Some T2, Some freq, _, Some bw, Some plp, `Fine ->
           Some (id, { standard = T2; channel = { freq; bw; plp }})
        | Some T, Some freq, _, Some bw, _, `Fine ->
           Some (id, { standard = T; channel = { freq; bw; plp = 0 }})
        | Some C, _, Some freq, Some bw, _, `Fine ->
           Some (id, { standard = C; channel = { freq; bw; plp = 0 }})
        | _ -> None)
      std#s_selected_value
      t_freq#s_selected_value
      c_freq#s_selected_value
      bw#s_selected_value
      plp#s_input
      state in
  let s_set =
    React.S.hold init ~eq:(Equal.option equal_mode) event
    |> React.S.map ~eq:Equal.unit (fun (m : mode option) ->
        (* NOTE standard should be updated first *)
        set_std m;
        set_t_freq m;
        set_c_freq m;
        set_bw m;
        set_plp m) in
  let s_dis =
    React.S.map ~eq:Equal.unit (fun x ->
        let is_disabled = match x with
          | `Fine -> false
          | _ -> true in
        std#set_disabled is_disabled;
        t_freq#set_disabled is_disabled;
        c_freq#set_disabled is_disabled;
        bw#set_disabled is_disabled;
        plp#set_disabled is_disabled)
      state
  in
  let submit = fun (id, m) ->
    Requests.Device.HTTP.set_mode ~id m control >|= (fun _ -> ()) in
  box, s, submit, (fun () ->
    React.S.stop ~strong:true s_set;
    React.S.stop ~strong:true s_dis)

let default_config = { id = 0 }

let name conf =
  Printf.sprintf "Модуль %d. Настройки"
    (succ (Option.get_or ~default:default_config conf).id)

let settings = None

(* FIXME declare class instead *)
let make ~(state : Common.Topology.state React.signal)
      ~(config : config React.signal)
      (conf : widget_config option)
      control =
  let conf = Option.get_or ~default:default_config conf in
  let get = List.Assoc.get ~eq:(=) conf.id in
  let w, s, set, close =
    make_mode_box ~id:conf.id
      ~init:(get @@ React.S.value config)
      ~event:(React.S.map ~eq:(Equal.option equal_mode) get config
              |> React.S.changes)
      ~state
      control
      () in
  let apply = new Ui_templates.Buttons.Set.t s set () in
  let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
  let actions = new Card.Actions.t ~widgets:[buttons] () in
  let box = new Vbox.t ~widgets:[w#widget; actions#widget] () in
  box#add_class base_class;
  box#set_on_destroy close;
  box#widget
