open Js_of_ocaml
open Application_types
open Board_niitv_dvb_types.Device
open Board_niitv_dvb_http_js
open Components

type widget_config =
  { id : int
  } [@@deriving yojson]

let base_class = "dvb-niit-module-settings"

let standard = Select.(
    Custom { to_string = standard_to_string ~full:true
           ; of_string = fun x ->
               match standard_of_string x with
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
  let signal, push = React.S.create None in
  let items = Select.native_options_of_values
      ~with_empty:true
      standard
      [T2; T; C] in
  let mode =
    Select.make_native
      ~on_change:(fun s -> push s#value)
      ~label:"Стандарт"
      ~items
      standard in
  let set = function
    | None -> mode#set_selected_index 0
    | Some x -> mode#set_value x.standard in
  mode#add_class @@ Components_tyxml.BEM.add_element base_class "mode";
  mode, set, signal

let make_bw (standard : standard option React.signal) =
  let signal, push = React.S.create None in
  let items = Select.native_options_of_values
      ~with_empty:true
      bw [Bw6; Bw7; Bw8] in
  let bw =
    Select.make_native
      ~on_change:(fun s -> push s#value)
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
    | Some (x : mode) -> bw#set_value x.channel.bw in
  bw#set_on_destroy (fun () -> React.S.stop ~strong:true s_hide);
  bw, set, signal

let make_freq ?(terrestrial = true) (standard : standard option React.signal) =
  let signal, push = React.S.create None in
  let items = Select.native_options_of_values
      ~with_empty:true
      ~label:(fun x -> "")
      Integer
      (List.map (fun (c : Channel.t) -> c.freq)
         (if terrestrial
          then Channel.Terrestrial.lst
          else Channel.Cable.lst)) in
  let freq =
    Select.make_native
      ~on_change:(fun s -> push s#value)
      ~label:"ТВ канал"
      ~items
      Integer in
  let s_hide =
    React.S.map ~eq:(=) (fun x ->
        match x, terrestrial with
        | (Some T, true) | (Some T2, true) | (Some C, false) ->
          freq#root##.style##.display := Js_of_ocaml.Js.string ""
        | _ -> freq#root##.style##.display := Js_of_ocaml.Js.string "none")
      standard in
  let set = function
    | None -> freq#set_selected_index 0
    | Some x ->
      match x.standard, terrestrial with
      | (T, true) | (T2, true) | (C, false) -> freq#set_value x.channel.freq
      | _ -> ()
  in
  freq#set_on_destroy (fun () -> React.S.stop ~strong:true s_hide);
  freq, set, signal

let make_plp (standard : standard option React.signal) =
  let signal, push = React.S.create None in
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
  plp#set_on_destroy (fun () -> React.S.stop ~strong:true s_hide);
  plp, set, signal

type event =
  [ `Mode of (int * mode) list
  | `State of Topology.state
  ]

let make_mode_box ~(id : int)
    ~(mode : (int * mode) list)
    ~(state : Topology.state)
    (control : int) =
  let std, set_std, s_std = make_standard () in
  let t_freq, set_t_freq, s_t_freq =
    make_freq ~terrestrial:true s_std in
  let c_freq, set_c_freq, s_c_freq =
    make_freq ~terrestrial:false s_std in
  let bw, set_bw, s_bw = make_bw s_std in
  let plp, set_plp, s_plp = make_plp s_std in
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
      s_std s_t_freq s_c_freq s_bw s_plp state in
  object(self)
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#append_child std;
      super#append_child t_freq;
      super#append_child c_freq;
      super#append_child bw;
      super#append_child plp;
      self#notify (`Mode mode)

    method! destroy () : unit =
      super#destroy ();
      std#destroy ();
      t_freq#destroy ();
      c_freq#destroy ();
      bw#destroy ();
      plp#destroy ();
      React.S.stop ~strong:true state;
      React.S.stop ~strong:true s_std;
      React.S.stop ~strong:true s_t_freq;
      React.S.stop ~strong:true s_c_freq;
      React.S.stop ~strong:true s_bw;
      React.S.stop ~strong:true s_plp;
      React.S.stop ~strong:true s

    method value : (int * mode) option =
      match std#value, t_freq#value, c_freq#value,
            bw#value, plp#value, React.S.value state with
      | Some T2, Some freq, _, Some bw, Some plp, `Fine ->
        Some (id, { standard = T2; channel = { freq; bw; plp }})
      | Some T, Some freq, _, Some bw, _, `Fine ->
        Some (id, { standard = T; channel = { freq; bw; plp = 0 }})
      | Some C, _, Some freq, Some bw, _, `Fine ->
        Some (id, { standard = C; channel = { freq; bw; plp = 0 }})
      | _ -> None

    method notify : event -> unit = function
      | `Mode mode ->
        let m = List.assoc_opt id mode in
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

    method s = s
  end

let default_config = { id = 0 }

let name config : string =
  Printf.sprintf "Модуль %d. Настройки" (succ config.id)

let settings = None

let ( >>= ) = Lwt.( >>= )

class t config mode state control =
  let mode_box = make_mode_box ~id:config.id ~mode ~state control in
  let submit = Button.make
      ~on_click:(fun btn _ _ ->
          match mode_box#value with
          | None -> Lwt.return_unit
          | Some (id, mode) ->
            let t =
              Http_receivers.set_mode ~id mode control
              >>= fun _ -> Lwt.return_unit in
            btn#set_loading_lwt t;
            t)
      ~label:"Применить"
      () in
  let buttons = Card.Actions.make_buttons [submit] in
  let actions = Card.Actions.make [buttons] in
  object
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#append_child mode_box;
      super#append_child actions;
      super#add_class base_class

    method notify event =
      mode_box#notify event

  end

let make config mode state control =
  new t config mode state control
