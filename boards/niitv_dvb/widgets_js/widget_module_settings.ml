open Js_of_ocaml
open Application_types
open Board_niitv_dvb_types.Device
open Board_niitv_dvb_http_js
open Components
open Lwt_result.Infix

type widget_config =
  { id : int
  } [@@deriving yojson]

let base_class = "dvb-niit-module-settings"

let standard = Select.(
    Custom { to_string = standard_to_string ~full:true
           ; of_string = fun x ->
               match standatd_of_string x with
               | None -> Error "Bad standard value"
               | Some x -> Ok x
           })

let bw =
  let to_string = function
    | Bw8 -> "8 МГц"
    | Bw7 -> "7 МГц"
    | Bw6 -> "6 МГц" in
  let of_string = function
    | "8 МГц" -> Ok Bw8
    | "7 МГц" -> Ok Bw7
    | "6 МГц" -> Ok Bw6
    | _ -> Error "Bad bw value" in
  Select.Custom { to_string; of_string }

let make_standard () =
  let items = Select.native_options_of_values
      ~with_empty:true
      standard
      [T2; T; C] in
  let mode =
    Select.make_native
      ~label:"Стандарт"
      ~items
      standard in
  let set = function
    | None -> mode#set_selected_index 0
    | Some x -> mode#set_value x.standard in
  mode#add_class @@ Components_tyxml.BEM.add_element base_class "mode";
  mode, set

let make_bw (standard : standard option React.signal) () =
  let items = Select.native_options_of_values
      ~with_empty:true
      bw [Bw6; Bw7; Bw8] in
  let bw =
    Select.make_native
      ~label:"Полоса пропускания"
      ~items
      bw in
  let s_hide =
    React.S.map ~eq:(=) (function
        | None -> bw#root##.style##.display := Js_of_ocaml.Js.string "none"
        | Some _ -> bw#root##.style##.display := Js_of_ocaml.Js.string "")
      standard in
  let set = function
    | None -> bw#set_selected_index 0
    | Some x -> bw#set_value x in
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
    React.S.map ~eq:(=) (fun x ->
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
    Textfield.make_textfield
      ~label:"PLP ID"
      ~required:true
      (Integer ((Some 0),(Some 255))) in
  plp#set_required true;
  let s_hide =
    React.S.map ~eq:(=) (function
        | Some T2 -> plp#root##.style##.display := Js_of_ocaml.Js.string ""
        | _ -> plp#root##.style##.display := Js_of_ocaml.Js.string "none") standard in
  let set = function
    | None -> plp#clear ()
    | Some x -> plp#set_value x.channel.plp in
  plp, set, (fun () -> React.S.stop ~strong:true s_hide)

type event =
  [ `Mode of (int * mode) list
  | `State of Topology.state
  ]

let make_mode_box ~(id : int)
    ~(mode : (int * mode) list)
    ~(state : Topology.state)
    (control : int) =
  let std, set_std = make_standard () in
  let t_freq, set_t_freq, t_freq_close =
    make_freq ~terrestrial:true std#s_selected_value () in
  let c_freq, set_c_freq, c_freq_close =
    make_freq ~terrestrial:false std#s_selected_value () in
  let bw, set_bw, bw_close =
    make_bw std#s_selected_value () in
  let plp, set_plp, plp_close =
    make_plp std#s_selected_value () in
  let state, set_state = React.S.create state in
  let s =
    let eq = Util_equal.(Option.equal (Pair.equal (=) equal_mode)) in
    React.S.l6 ~eq
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
  let submit = fun (id, m) ->
    Http_receivers.set_mode ~id m control
    >|= (fun _ -> ()) in
  object(self)
    inherit Vbox.t
        ~widgets:[ std#widget
                 ; t_freq#widget
                 ; c_freq#widget
                 ; bw#widget
                 ; plp#widget ]
        () as super

    method! init () : unit =
      super#init ();
      self#notify (`Mode mode)

    method! destroy () : unit =
      super#destroy ();
      t_freq_close ();
      c_freq_close ();
      bw_close ();
      plp_close ()

    method notify : event -> unit = function
      | `Mode mode ->
        let m = List.Assoc.get ~eq:(=) id mode in
        (* NOTE standard should be updated first *)
        set_std m;
        set_t_freq m;
        set_c_freq m;
        set_bw m;
        set_plp m
      | `State s ->
        set_state s;
        let is_disabled = match s with `Fine -> false | _ -> true in
        std#set_disabled is_disabled;
        t_freq#set_disabled is_disabled;
        c_freq#set_disabled is_disabled;
        bw#set_disabled is_disabled;
        plp#set_disabled is_disabled

    method submit = submit

    method s = s
  end

let default_config = { id = 0 }

let name config : string =
  Printf.sprintf "Модуль %d. Настройки" (succ config.id)

let settings = None

class t config mode state control =
  let mode_box = make_mode_box ~id:config.id ~mode ~state control in
  let submit = Button.make ~label:"Применить" () in
  let buttons = Card.Actions.make_buttons [submit] in
  let actions = Card.Actions.make [buttons] in
  object
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#append_child mode_box;
      super#append_child actions;
      super#add_class base_class

    (* method notify event =
     *   mode_box#notify event *)

  end

let make config mode state control =
  new t config mode state control
