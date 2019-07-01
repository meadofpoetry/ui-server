open Js_of_ocaml
open Js_of_ocaml_lwt
open Application_types
open Board_niitv_dvb_types.Device
open Board_niitv_dvb_http_js
open Components

type widget_config =
  { id : int
  } [@@deriving yojson]

type event =
  [ `Mode of mode option
  | `State of Topology.state
  | `PLPs of int list
  ]

let ( >>= ) = Lwt.bind

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

let make_standard ?value () =
  let signal, push = React.S.create None in
  let items = Select.native_options_of_values
      ~with_empty:true
      standard
      [T2; T; C] in
  let select =
    Select.make_native
      ?value
      ~on_change:(fun s -> push s#value)
      ~label:"Стандарт"
      ~items:(`Markup items)
      standard in
  select#add_class @@ BEM.add_element base_class "mode";
  select, signal

let make_bw ?value () =
  let event, push = React.E.create () in
  let items = Select.native_options_of_values
      ~with_empty:true
      bw [Bw6; Bw7; Bw8] in
  let bw =
    Select.make_native
      ?value
      ~on_change:(fun _ -> push ())
      ~label:"Полоса пропускания"
      ~items:(`Markup items)
      bw in
  bw, event

let contains pattern value =
  let len = String.length value in
  if len > String.length pattern
  then false
  else
    let sub = String.sub pattern 0 len in
    String.uppercase_ascii sub = String.uppercase_ascii value

let max_frequency = 950_000_000

let min_frequency = 50_000_000

(* let format_frequency (x : int) =
 *   let num = Js.number_of_float (float_of_int x) in
 *   num##toLocaleString
 * 
 * let frequency_to_string (x : int) =
 *   Js.to_string (format_frequency x)
 * 
 * let frequency_of_js_string (s : Js.js_string Js.t) =
 *   try
 *     let regexp = new%js Js.regExp_withFlags
 *       (Js.string "[\\D\\s\\._\\-]")
 *       (Js.string "g") in
 *     let v = s##replace regexp (Js.string "") in
 *     let x = Js.parseInt v in
 *     if Js.isNaN x then None else Some x
 *   with _ -> None
 * 
 * let frequency_of_string (s : string) =
 *   match frequency_of_js_string (Js.string s) with
 *   | None -> Error "Value must be an integer"
 *   | Some x ->
 *     if x > max_frequency
 *     then Error "Частота должна быть меньше 950 МГц"
 *     else if x < min_frequency
 *     then Error "Частота должна быть больше 50 МГц"
 *     else Ok x
 * 
 * let frequency = Textfield.(
 *     Custom { to_string = frequency_to_string
 *            ; of_string = frequency_of_string
 *            ; input_type = `Text
 *            }) *)

let handle_input
    (menu : Menu.t)
    (input : int Textfield.t)
    (standard : standard option React.signal) =
  (* let v = match frequency_of_js_string input#input_element##.value with
   *   | None -> 0
   *   | Some x -> x in
   * input#input_element##.value := format_frequency v; *)
  match (* string_of_int v *) input#value_as_string with
  | "" -> menu#close ()
  | v ->
    let matching = List.filter (fun (c : Channel.t) ->
        let freq = string_of_int c.freq in
        let chan = string_of_int c.chan in
        contains freq v || contains chan v)
      @@ match React.S.value standard with
      | None -> []
      | Some (T | T2) -> Channel.Terrestrial.lst
      | Some C -> Channel.Cable.lst in
    Utils.Option.iter (fun x -> x#remove_children ()) menu#list;
    (match matching with
     | [] -> menu#close ()
     | x ->
       List.iter (fun (c : Channel.t) ->
           let item = Item_list.Item.make c.name in
           item#set_attribute "value" (string_of_int c.freq);
           Utils.Option.iter (fun (x : Item_list.t) ->
               x#append_child item) menu#list) x;
       menu#reveal ())

let make_frequency ?(value : int option) (standard : standard option React.signal) =
  let event, push = React.E.create () in
  let trailing_icon = Typography.Text.make "Гц" in
  let list = Item_list.make [] in
  let menu = Menu.make_of_item_list ~focus_on_open:false list in
  let input =
    Textfield.make_textfield
      ?value
      ~required:true
      ~input_mode:`Numeric
      ~max_length:11
      ~on_input:(fun e input -> push (); handle_input menu input standard)
      ~trailing_icon
      ~label:"Частота"
      (Integer (Some min_frequency, Some max_frequency)) in
  let active_class = Item_list.CSS.item_activated in
  let apply_selected item =
    let v = item##getAttribute (Js.string "value") in
    Js.Opt.iter v (fun v -> input#input_element##.value := v);
    (* Js.Opt.iter v (fun v -> match frequency_of_js_string v with
     *     | None -> ()
     *     | Some x -> input#input_element##.value := format_frequency x); *)
    menu#close () in
  let get_selected items =
    List.find_opt (fun x -> Element.has_class x active_class) items in
  let next dir e =
    match menu#is_open, menu#items with
    | false, _ | _, [] -> Lwt.return_unit
    | true, items ->
      Dom.preventDefault e;
      let next = match get_selected items with
        | None -> List.hd items
        | Some x ->
          Element.remove_class x active_class;
          let x = Js.Unsafe.coerce x in
          match dir with
          | `Down -> Js.Opt.get x##.nextElementSibling (fun () -> List.hd items)
          | `Up ->
            Js.Opt.get x##.previousElementSibling (fun () ->
                List.hd @@ List.rev items) in
      next##scrollIntoView Js._false;
      Element.add_class next active_class;
      Lwt.return_unit in
  let keydown = Events.keydowns input#input_element (fun e _ ->
      match Dom_html.Keyboard_code.of_event e with
      | Space -> Dom.preventDefault e; Lwt.return_unit
      | Escape -> menu#close ()
      | ArrowDown -> next `Down e
      | ArrowUp -> next `Up e
      | Enter -> begin match menu#is_open, get_selected menu#items with
          | true, Some item ->
            Dom.preventDefault e;
            apply_selected item
          | _ -> Lwt.return_unit
        end
      | _ -> Lwt.return_unit) in
  let open_ =
    Lwt_js_events.seq_loop (Lwt_js_events.make_event Menu.Event.opened)
      menu#root (fun _ _ ->
          let closed = Lwt_js_events.make_event Menu.Event.closed menu#root in
          let select = Lwt_js_events.make_event Menu.Event.selected menu#root in
          input#set_use_native_validation false;
          let t =
            Lwt.pick
              [ (select >>= fun e -> Lwt.return @@ `Selected e)
              ; (closed >>= fun _ -> Lwt.return `Closed) ] in
          Lwt.on_termination t (fun () -> input#set_use_native_validation true);
          t >>= function
          | `Closed -> Lwt.return_unit
          | `Selected e ->
            input#set_valid input#valid;
            apply_selected (Widget.event_detail e)##.item) in
  menu#set_quick_open true;
  menu#set_anchor_element input#root;
  menu#set_anchor_corner Bottom_left;
  menu#set_width_as_anchor true;
  menu#hoist_menu_to_body ();
  input#set_on_destroy (fun () ->
      List.iter Lwt.cancel [open_; keydown];
      Element.remove_child_safe Dom_html.document##.body menu#root;
      menu#destroy ());
  input, event

let make_plp ?value (plps : int list React.signal) =
  let event, push = React.S.create None in
  let list = Item_list.make [] in
  let menu = Menu.make_of_item_list ~focus_on_open:false list in
  let trailing_icon = Icon.SVG.(make_simple Path.menu_down) in
  trailing_icon#set_attribute "tabindex" "0";
  let input =
    Textfield.make_textfield
      ?value
      ~trailing_icon
      ~on_input:(fun _ i -> push i#value; Lwt.return_unit)
      ~label:"PLP ID"
      ~required:true
      (Integer ((Some 0), (Some 255))) in
  let set_menu_toggle_disabled disabled =
    match input#trailing_icon with
    | None -> ()
    | Some x -> x#set_disabled disabled in
  input#set_required true;
  menu#set_quick_open true;
  menu#set_anchor_element input#root;
  menu#set_anchor_corner Bottom_left;
  menu#set_width_as_anchor true;
  menu#hoist_menu_to_body ();
  let s_plps = React.S.map (function
      | [] ->
        set_menu_toggle_disabled true;
        list#remove_children ()
      | plps ->
        let items = List.map (fun x ->
            let item = Item_list.Item.make @@ string_of_int x in
            item#set_attribute "value" (string_of_int x);
            item) plps in
        set_menu_toggle_disabled false;
        List.iter list#append_child items) plps in
  let selected = Lwt_js_events.seq_loop
      (Lwt_js_events.make_event Menu.Event.selected)
      menu#root (fun e _ ->
          let item = (Widget.event_detail e)##.item in
          Utils.Option.iter input#set_value_as_string
          @@ Element.get_attribute item "value";
          Lwt.return_unit) in
  let icon_click = Lwt_js_events.seq_loop
      (Lwt_js_events.make_event Textfield.Event.icon)
      input#root (fun _ _ -> menu#reveal ()) in
  input#set_on_destroy (fun () ->
      React.S.stop ~strong:true s_plps;
      Lwt.cancel selected;
      Lwt.cancel icon_click;
      Element.remove_child_safe Dom_html.document##.body menu#root;
      menu#destroy ());
  input, event

let make_mode_box ~(id : int) (state : Topology.state) (mode : mode option) plps =
  let plps, push_plps = React.S.create plps in
  let std, s_std = make_standard
      ?value:(Utils.Option.map (fun x -> x.standard) mode)
      () in
  let freq, e_freq = make_frequency
      ?value:(Utils.Option.map (fun x -> x.channel.freq) mode)
      s_std in
  let bw, e_bw = make_bw
      ?value:(Utils.Option.map (fun x -> x.channel.bw) mode)
      () in
  let plp, e_plp = make_plp
      ?value:(Utils.Option.map (fun x -> x.channel.plp) mode)
      plps in
  object(self)
    val mutable _state = state
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#append_child std;
      super#append_child freq;
      super#append_child bw;
      super#append_child plp;
      super#add_class Box.CSS.root;
      super#add_class Box.CSS.vertical;
      self#notify (`State state)

    method! destroy () : unit =
      super#destroy ();
      std#destroy ();
      freq#destroy ();
      bw#destroy ();
      plp#destroy ()

    method value : (int * mode) option =
      match _state, std#value, freq#value, bw#value, plp#value with
      | `Fine, Some T2, Some freq, Some bw, Some plp ->
        Some (id, { standard = T2; channel = { freq; bw; plp }})
      | `Fine, Some T, Some freq, Some bw, _ ->
        Some (id, { standard = T; channel = { freq; bw; plp = 0 }})
      | `Fine, Some C, Some freq, Some bw, _ ->
        Some (id, { standard = C; channel = { freq; bw; plp = 0 }})
      | _ -> None

    method notify : event -> unit = function
      | `PLPs x -> push_plps x
      | `Mode mode ->
        (match mode with
         | None ->
           std#set_selected_index 0;
           bw#set_selected_index 0;
           freq#clear ();
           plp#clear ();
         | Some mode ->
           std#set_value mode.standard;
           bw#set_value mode.channel.bw;
           freq#set_value mode.channel.freq;
           plp#set_value mode.channel.plp)
      | `State s ->
        _state <- s;
        let is_disabled = match s with `Fine -> false | _ -> true in
        std#set_disabled is_disabled;
        freq#set_disabled is_disabled;
        bw#set_disabled is_disabled;
        plp#set_disabled is_disabled

    (* method s = s *)
  end

let ( >>= ) = Lwt.( >>= )

class t config state mode plps control =
  let mode_box = make_mode_box ~id:config.id state mode plps in
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

    method! destroy () : unit =
      mode_box#destroy ();
      super#destroy ()

    method id = config.id

    method notify (event : event) =
      mode_box#notify event

  end

let make config state mode plps control =
  new t config state mode plps control
