open Containers
open Components
open Lwt.Infix
open Application_types
open Common

let dense = false

let base_class = "application-stream-selector"
let inputs_class = CSS.add_element base_class "inputs"
let stream_class = CSS.add_element base_class "stream"
let lost_class = CSS.add_modifier stream_class "lost"
let block_class = CSS.add_element base_class "block"
let forbidden_class = CSS.add_modifier block_class "forbidden"
let dialog_class = CSS.add_element base_class "dialog"
let empty_placeholder_class = CSS.add_element base_class "empty-placeholder"

type check =
  { avail : bool React.signal
  ; enable : unit -> unit
  ; disable : unit -> unit
  }

type stream_dialog =
  { dialog : Dialog.t
  ; show : unit -> Dialog.action Lwt.t
  ; result : (Stream.t, string) result React.signal
  }

let make_empty_placeholder () =
  new Typography.Text.t ~text:"Нет потоков" ()

class base ?actions ~body
        ?(left : int React.signal option)
        ~(state : Stream.Table.source_state)
        ~(entry : Topology.topo_entry) () =
  let title = match entry with
    | Topology.Input i -> "Вход " ^ Topology.get_input_name i
    | Topology.Board b -> "Плата " ^ Topology.get_board_name b in
  let title = new Card.Primary.title title () in
  let subtitle = match left with
    | None -> None
    | Some _ -> Some (new Card.Primary.subtitle "" ()) in
  let primary_widgets = match subtitle with
    | None -> [title]
    | Some st -> [title; st] in
  let primary = match primary_widgets with
    | [] -> None
    | widgets -> Some (new Card.Primary.t ~widgets ()) in
  let media = new Card.Media.t ~widgets:[body] () in
  let actions = match actions with
    | None -> None
    | Some a -> Some (new Card.Actions.t ~widgets:a ()) in
  let widgets = match primary, actions with
    | None, None -> [media#widget]
    | Some p, None -> [p#widget; media#widget]
    | None, Some a -> [media#widget; a#widget]
    | Some p, Some a -> [p#widget; media#widget; a#widget] in
  let s = match left with
    | None -> React.S.const ()
    | Some s ->
       React.S.map ~eq:Equal.unit (fun n ->
           let s = Printf.sprintf "Лимит: %d" n in
           Option.iter (fun st -> st#set_text_content s) subtitle) s in
  object

    inherit Card.t
              ~outlined:true
              ~widgets
              () as super

    method! init () : unit =
      super#init ();
      super#add_class block_class;
      begin match state with
      | `Forbidden -> super#add_class forbidden_class
      | _ -> ()
      end

    method! destroy () : unit =
      super#destroy ();
      React.S.stop ~strong:true s

  end

module Board = struct

  let make_item ?(url = None)
        ~(check : check)
        ~(present : bool)
        (stream : Stream.t) =
    let text = Stream.Source.to_string stream.source.info in
    let checkbox = new Checkbox.t () in
    checkbox#set_checked @@ Option.is_some url;
    let _s =
      React.S.map ~eq:Equal.unit
        (fun v -> checkbox#set_disabled (not v)) check.avail in
    let _e =
      React.S.diff (fun s _ ->
          if s then check.enable () else check.disable ())
        checkbox#s_state in
    let s =
      React.S.map ~eq:(Equal.option Stream.equal)
        (fun s -> if s then Some stream else None)
        checkbox#s_state in
    let o =
      object

        val mutable _click_listener = None

        inherit [unit] Item_list.Item.t
                  ~text
                  ~meta:checkbox
                  ~value:()
                  () as super

        method! init () : unit =
          super#init ();
          super#add_class stream_class;
          if not present then super#add_class lost_class;
          let l =
            super#listen_click_lwt (fun e _ ->
                let target = Js_of_ocaml.Js.Opt.to_option e##.target in
                let is_checkbox =
                  Option.map_or ~default:false
                    (fun e ->
                      let elt = (checkbox#input_element :> Widget.element) in
                      Equal.physical e elt)
                    target in
                if not is_checkbox
                then
                  if not checkbox#disabled
                  then checkbox#toggle ();
                Lwt.return_unit) in
          _click_listener <- Some l

        method! destroy () : unit =
          super#destroy ();
          Option.iter Lwt.cancel _click_listener;
          _click_listener <- None;
          React.S.stop ~strong:true _s;
          React.S.stop ~strong:true s;
          React.E.stop ~strong:true _e;

      end in
    o, s

  let make_list state counter counter_push
        (stream_list : Stream.Table.stream list) =
    let open Stream.Table in
    let available = match state with
      | `Forbidden -> React.S.const false
      | `Unlimited -> React.S.const true
      | `Limited lim ->
         React.S.map ~eq:Bool.equal
           (fun counter -> not (counter > lim)) counter in
    let check =
      { avail = available
      ; enable = (fun () -> counter_push @@ succ @@ React.S.value counter)
      ; disable = (fun () -> counter_push @@ pred @@ React.S.value counter)
      } in
    let items, stream_signals =
      List.split
      @@ List.map (fun ({ url; stream; present } : stream) ->
             make_item ~check ~url ~present stream)
           stream_list in
    let settings =
      let eq = Equal.list Stream.equal in
      React.S.merge ~eq:(Equal.list (Equal.option Stream.equal))
        (fun acc v -> v :: acc) [] stream_signals
      |> React.S.map ~eq (List.filter_map Fun.id) in
    let non_interactive = match state with
      | `Forbidden -> true | _ -> false in
    let list =
      new Item_list.t
        ~non_interactive
        ~dense
        ~items:(List.map (fun i -> `Item i) items) () in
    list, settings

  class t (state : Stream.Table.source_state)
          (list : unit Item_list.t)
          (settings : Stream.t list React.signal)
          (counter : int React.signal)
          (left : int React.signal option)
          (entry : Topology.topo_entry)
          () =
    let empty = make_empty_placeholder () in
    let body = Widget.create_div () in
    object(self)

      inherit base ?left ~body ~entry ~state () as super

      method! init () : unit =
        super#init ();
        empty#add_class empty_placeholder_class;
        React.S.map ~eq:Equal.unit self#check_empty list#s_items
        |> super#_keep_s

      method! destroy () : unit =
        super#destroy ();
        React.S.stop ~strong:true counter;
        React.S.stop ~strong:true settings;
        Option.iter (React.S.stop ~strong:true) left

      method private check_empty items : unit =
        match items with
        | [] -> body#remove_child list;
                body#append_child empty
        | _ -> body#remove_child empty;
               body#append_child list

    end

  let make_entry state counter counter_push left stream_list input =
    let list, settings = make_list state counter counter_push stream_list in
    let w = new t state list settings counter left (Topology.Input input) () in
    w#widget, settings

  let make (board : Topology.topo_board option)
        ((marker, state, stream_list) : stream_table_row) =
    let open Stream.Table in
    let open Topology in
    let inputs =
      Option.map_or ~default:[]
        (fun b -> Topology.get_inputs (`Boards [b])) board in
    let counter, counter_push =
      let init_list =
        List.filter_map (function
            | ({ url = Some _; stream; _ } : stream) -> Some stream
            | { url = None;  _ } -> None) stream_list in
      React.S.create ~eq:Int.equal (List.length init_list) in
    let left = match state with
      | `Forbidden | `Unlimited -> None
      | `Limited lim ->
         Some (React.S.map ~eq:(=) (fun x -> lim - x) counter) in
    let w, settings =
      List.map (fun (i : topo_input) ->
          let filtered_streams =
            List.filter (fun (s : stream) ->
                match Stream.get_input s.stream with
                | None -> false
                | Some input -> equal_topo_input i input)
              stream_list in
          make_entry state counter counter_push left filtered_streams i) inputs
      |> List.split in
    let eq_l = Equal.list Stream.equal in
    let eq = Equal.pair equal_marker eq_l in
    let settings =
      React.S.merge ~eq:eq_l (@) [] settings
      |> React.S.map ~eq (Pair.make marker) in
    w, settings

end

module Input = struct

  module Stream_dialog = struct

    let merge input addr port =
      let open Stream in
      match addr, port with
      | Some (addr : Ipaddr.V4.t), Some (port : int) ->
         let source =
           { info = IPV4 { scheme = "udp"
                         ; addr
                         ; port }
           ; node = Entry (Input input) } in
         Ok { id = make_id source
            ; orig_id = TSoIP { addr; port }
            ; typ = TS
            ; source
           }
      | _ -> Error "no data provided"

    let make (input : Topology.topo_input) =
      let open Stream in
      let addr =
        new Textfield.t
          ~input_type:IPV4
          ~label:"IP адрес"
          () in
      let port =
        new Textfield.t
          ~input_type:(Integer ((Some 0), (Some 65535)))
          ~label:"UDP порт"
          () in
      let eq = Result.equal ~err:String.equal equal in
      let result = React.S.l2 ~eq (merge input)
                     addr#s_input port#s_input in
      let accept =
        new Ui_templates.Buttons.Set.t ~label:"применить"
          (React.S.map ~eq:(Equal.option equal)
             Result.to_opt result)
          (fun _ -> Lwt.return_unit)
          () in
      let cancel = new Button.t ~label:"отмена" () in
      let box = new Vbox.t ~widgets:[addr#widget; port#widget] () in
      let dialog =
        new Dialog.t
          ~title:"Добавление потока"
          ~actions:[ Dialog.Action.make ~typ:`Accept accept
                   ; Dialog.Action.make ~typ:`Cancel cancel ]
          ~content:(`Widgets [box#widget])
          () in
      dialog#add_class dialog_class;
      let show () = dialog#show_await () in
      { dialog; show; result }

    let show dialog streams =
      dialog.show ()
      >>= function
      | `Cancel -> Lwt.return_error "dialog was canceled"
      | `Accept ->
         React.S.value dialog.result
         |> Result.flat_map (fun s ->
                if List.exists (Stream.equal s) (React.S.value streams)
                then Error "streams exists" else Ok s)
         |> Lwt.return

  end

  let make_stream_list stream_list =
    let make_board_stream_entry del_item del_stream (stream : Stream.t) =
      let text = Stream.Source.to_string stream.source.info in
      let icon = Icon.SVG.(create_simple Path.delete) in
      let del_button = Icon_button.make ~ripple:false ~icon () in
      let item =
        new Item_list.Item.t
          ~text
          ~meta:del_button
          ~value:()
          () in
      del_button#listen_click_lwt (fun _ _ ->
          del_item item;
          del_stream stream;
          Lwt.return_unit)
      |> Lwt.ignore_result;
      item in
    let signal, push =
      React.S.create ~eq:(Equal.list Stream.equal) stream_list in
    let list =
      new Item_list.t
        ~dense
        ~items:[]
        () in
    let del_item i = list#remove_item i in
    let del_stream s =
      let slst = React.S.value signal in
      push @@ List.filter (fun x -> not @@ Stream.equal s x) slst
    in
    let items = List.map (make_board_stream_entry del_item del_stream) stream_list in
    List.iter list#append_item items;
    let add stream =
      let slst = React.S.value signal in
      if List.exists (Stream.equal stream) slst
      then failwith "stream exists"; (* TODO fix *)
      let item = make_board_stream_entry del_item del_stream stream in
      list#append_item item;
      push (stream::slst)
    in
    signal, list, add

  class t ((iid, state, stream_list) : stream_table_row) () =
    let open Stream.Table in
    let input, id = match iid with
      | `Input (inp, id) -> inp, id
      | _ -> failwith "impossible" in
    let (topo_input : Topology.topo_input) = { input; id } in
    let init_list =
      List.filter_map (function
          | ({ url = Some _; stream; _ } : stream) -> Some stream
          | { url = None;  _ } -> None)
        stream_list in
    let dialog = Stream_dialog.make topo_input in
    let streams, list, add = make_stream_list init_list in
    let settings =
      React.S.map ~eq:(fun (x1, s1) (x2, s2) ->
          let eq_x = match x1, x2 with
            | `Input (ip1, i1), `Input (ip2, i2) ->
               Topology.equal_input ip1 ip2 && Int.equal i1 i2
            | _ -> false in
          let eq_s = (Equal.list Stream.equal) s1 s2 in
          eq_x && eq_s)
        (fun slst -> iid, slst) streams in
    let add = fun () ->
      Stream_dialog.show dialog streams >>= function
      | Error e -> Lwt.return @@ print_endline e
      | Ok s -> Lwt.return @@ add s in
    let apply =
      new Ui_templates.Buttons.Set.t
        ~label:"Добавить поток"
        (React.S.const @@ Some ())
        add
        () in
    let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
    let body = Widget.create_div () in
    let empty = make_empty_placeholder () in
    object(self)

      inherit base ~state
                ~body
                ~actions:[buttons]
                ~entry:(Input topo_input)
                () as super

      method! init () : unit =
        super#init ();
        empty#add_class empty_placeholder_class;
        React.S.map ~eq:Equal.unit self#check_empty list#s_items
        |> self#_keep_s;
        Widget.append_to_body dialog.dialog

      method! destroy () : unit =
        super#destroy ();
        Widget.remove_from_body dialog.dialog;
        React.S.stop ~strong:true settings

      method settings = settings

      method private check_empty items : unit =
        match items with
        | [] -> body#remove_child list;
                body#append_child empty
        | _ -> body#remove_child empty;
               body#append_child list

    end

  let make (row : stream_table_row) =
    let w = new t row () in
    [w#widget], w#settings

end

let make_entry : Topology.topo_cpu -> stream_table_row ->
                 (Widget.t list) * ((marker * Stream.t list) React.signal) =
  fun cpu st ->
  let (marker, b, sl) = st in
  let sl =
    List.sort (fun (a : Stream.Table.stream) b ->
        Stream.compare a.stream b.stream) sl in
  let st = (marker, b, sl) in
  match marker with
  | `Input _ -> Input.make st
  | `Board id ->
     let board =
       List.find_opt (fun (b : Topology.topo_board) ->
           b.control = id)
       @@ Topology.get_boards (`CPU cpu) in
     Board.make board st

let make_table cpu (table : stream_table) =
  let widgets, signals = List.split @@ List.map (make_entry cpu) table in
  let widgets = List.concat widgets in
  let list = new Vbox.t ~widgets () in
  list#set_on_destroy (fun () ->
      List.iter (fun w -> w#destroy ()) widgets);
  list, React.S.map ~eq:(Equal.option equal_stream_setting) Option.return
          (React.S.merge ~eq:equal_stream_setting (fun acc v -> v :: acc)
             [] signals)

class t ~(init : stream_table)
        ~(event : stream_table React.event)
        (cpu : Topology.topo_cpu)
        () =
  let id = "settings-place" in
  let post = Requests.HTTP.set_streams in
  let s_in = React.S.hold ~eq:equal_stream_table init event in
  let make (table : stream_table) =
    let dis, s = make_table cpu table in
    let place = dis in
    place#set_id id;
    place, s in
  let s_div =
    React.S.map ~eq:(fun (w1, ss1) (w2, ss2) ->
        let eq_w = Widget.equal in
        let eq_ss = React.S.equal ~eq:(Equal.option equal_stream_setting) in
        eq_w w1 w2 && eq_ss ss1 ss2)
      (fun s -> make s) s_in in
  let e_div =
    React.S.diff (fun _ (o, _) ->
        o#destroy ()) s_div in
  let div = Widget.create_div () in
  let s =
    let eq = Equal.option equal_stream_setting in
    React.S.switch ~eq
      (React.S.map ~eq:(React.S.equal ~eq)
         (fun n ->
           div#set_empty ();
           let w, n_s = n in
           div#append_child w;
           n_s) s_div) in
  let apply = new Ui_templates.Buttons.Set.t s post () in
  let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
  let actions = new Card.Actions.t ~widgets:[buttons] () in
  object(self)

    inherit Vbox.t ~widgets:[div; actions#widget] () as super

    method! init () : unit =
      super#init ();
      div#add_class inputs_class;
      self#add_class base_class;

    method! destroy () : unit =
      super#destroy ();
      apply#destroy ();
      actions#destroy ();
      buttons#destroy ();
      React.E.stop ~strong:true e_div;
      React.S.stop ~strong:true s;
      React.S.stop ~strong:true s_div;
      React.S.stop ~strong:true s_in

  end

let make ~init ~event cpu () =
  new t ~init ~event cpu ()
