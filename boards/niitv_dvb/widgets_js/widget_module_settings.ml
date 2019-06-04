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

let filter_frequency_string (s : string) =
  List.filter (function
      | '0'..'9' -> true
      | _ -> false)
  @@ List.of_seq
  @@ String.to_seq s

let format_frequency ?(filter = true) (s : string) =
  let s =
    if filter
    then filter_frequency_string s
    else List.of_seq @@ String.to_seq s in
  let rec loop acc = function
    | [] -> acc
    | a :: b :: c :: (_ :: _ as tl) ->
      let acc = ' ' :: c :: b :: a :: acc in
      loop acc tl
    | l -> List.rev l @ acc in
  String.of_seq @@ List.to_seq @@ loop [] @@ List.rev s

let max_frequency = 950_000_000

let min_frequency = 50_000_000

let frequency_to_string (x : int) =
  format_frequency (string_of_int x)

let frequency_of_string (s : string) =
  let s = String.of_seq @@ List.to_seq @@ filter_frequency_string s in
  match int_of_string_opt s with
  | None -> Error "Bad frequency format"
  | Some i ->
    if i > max_frequency
    then Error "Частота не может превышать 950 МГц"
    else if i < min_frequency
    then Error "Частота не может быть меньше 50 МГц"
    else Ok i

let frequency = Textfield.(
    let to_string = frequency_to_string in
    let of_string = frequency_of_string in
    Custom { to_string
           ; of_string
           ; input_type = `Text
           })

let make_frequency ?(value : int option) (standard : standard option React.signal) =
  let event, push = React.E.create () in
  let trailing_icon = Typography.Text.make "Гц" in
  let list = Item_list.make [] in
  let menu = Menu.make_of_item_list ~focus_on_open:false list in
  let input =
    Textfield.make_textfield
      ?value
      ~max_length:11
      ~on_input:(fun input ->
          push ();
          let v =
            String.of_seq
            @@ List.to_seq
            @@ filter_frequency_string input#value_as_string in
          input#input_element##.value := Js.string (format_frequency ~filter:false v);
          match v with
          | "" -> Lwt.async menu#close
          | v ->
            let matching = List.filter (fun (c : Channel.t) ->
                let freq = string_of_int c.freq in
                let chan = string_of_int c.chan in
                contains freq v || contains chan v)
              @@ match React.S.value standard with
              | None -> []
              | Some (T | T2) -> Channel.Terrestrial.lst
              | Some C -> Channel.Cable.lst in
            list#remove_children ();
            (match matching with
             | [] -> Lwt.async menu#close
             | x ->
               List.iter (fun (c : Channel.t) ->
                   let item = Item_list.Item.make c.name in
                   item#set_attribute "value" (string_of_int c.freq);
                   list#append_child item) x;
               Lwt.async menu#reveal))
      ~trailing_icon
      ~label:"Частота"
      frequency in
  let active_class = Item_list.CSS.item_activated in
  let get_selected items =
    List.find_opt (fun x -> Element.has_class x active_class) items in
  let next dir = function
    | [] -> ()
    | items ->
      let next = match get_selected items with
        | None -> List.hd items
        | Some x ->
          Element.remove_class x active_class;
          let x = Js.Unsafe.coerce x in
          match dir with
          | `Down -> Js.Opt.get x##.nextElementSibling (fun () -> List.hd items)
          | `Up -> Js.Opt.get x##.previousElementSibling (fun () ->
              List.hd @@ List.rev items) in
      next##scrollIntoView Js._false;
      Element.add_class next active_class in
  let keydown = Events.keydowns input#input_element (fun e _ ->
      let items = list#items in
      match Events.Key.of_event e with
      | `Space -> Dom.preventDefault e; Lwt.return_unit
      | `Escape -> menu#close ()
      | `Arrow_down ->
        if menu#is_open then (Dom.preventDefault e; next `Down items);
        Lwt.return_unit
      | `Arrow_up ->
        if menu#is_open then (Dom.preventDefault e; next `Up items);
        Lwt.return_unit
      | `Enter ->
        (match menu#is_open, get_selected items with
         | true, Some item ->
           Dom.preventDefault e;
           (match Element.get_attribute item "value" with
            | None -> Lwt.return_unit
            | Some v ->
              input#set_value_as_string v;
              menu#close ())
         | _ -> Lwt.return_unit)
      | _ -> Lwt.return_unit) in
  let open_ = Events.listen_lwt menu#root Menu.Event.opened (fun _ _ ->
      input#set_use_native_validation false;
      let closed = Events.make_event Menu.Event.closed menu#root in
      let select = Events.make_event Menu.Event.selected menu#root in
      let t =
        Lwt.pick
          [ (select >>= fun e -> Lwt.return @@ `Selected e)
          ; (closed >>= fun _ -> Lwt.return `Closed) ]
        >>= function
        | `Closed -> Lwt.return_unit
        | `Selected e ->
          (match Element.get_attribute (Widget.event_detail e)##.item "value" with
           | None -> ()
           | Some value -> input#set_value_as_string (format_frequency value));
          Lwt.return_unit in
      Lwt.on_termination t (fun () ->
          input#set_use_native_validation true;
          input#set_valid input#valid);
      t) in
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
      ~on_input:(fun i -> push i#value)
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
  let selected = Events.listen_lwt menu#root Menu.Event.selected (fun e _ ->
      let item = (Widget.event_detail e)##.item in
      Utils.Option.iter input#set_value_as_string
      @@ Element.get_attribute item "value";
      Lwt.return_unit) in
  let icon_click = Events.listen_lwt input#root Textfield.Event.icon (fun _ _ ->
      menu#reveal ()) in
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
