open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Lwt.Infix
open Application_types

let dense = false

let base_class = "application-stream-selector"
let inputs_class = BEM.add_element base_class "inputs"
let stream_class = BEM.add_element base_class "stream"
let lost_class = BEM.add_modifier stream_class "lost"
let block_class = BEM.add_element base_class "block"
let forbidden_class = BEM.add_modifier block_class "forbidden"
let dialog_class = BEM.add_element base_class "dialog"
let empty_placeholder_class = BEM.add_element base_class "empty-placeholder"

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
  Typography.Text.make "Нет потоков"

class base ?actions ~body
    ?(left : int React.signal option)
    ~(state : Stream.Table.source_state)
    ~(entry : Topology.topo_entry) () =
  let title = match entry with
    | Topology.Input i -> "Вход " ^ Topology.get_input_name i
    | Topology.Board b ->
      Printf.sprintf "Плата %s %s v%d" b.manufacturer b.model b.version in
  let title = Card.Primary.make_title title in
  let subtitle = match left with
    | None -> None
    | Some _ -> Some (Card.Primary.make_subtitle "") in
  let primary_widgets = match subtitle with
    | None -> [title]
    | Some st -> [title; st] in
  let primary = match primary_widgets with
    | [] -> None
    | widgets -> Some (Card.Primary.make widgets) in
  let media = Card.Media.make [body] in
  let actions = match actions with
    | None -> None
    | Some a -> Some (Card.Actions.make a) in
  let widgets = match primary, actions with
    | None, None -> [media#widget]
    | Some p, None -> [p#widget; media#widget]
    | None, Some a -> [media#widget; a#widget]
    | Some p, Some a -> [p#widget; media#widget; a#widget] in
  let s = match left with
    | None -> React.S.const ()
    | Some s ->
      React.S.map ~eq:(=) (fun n ->
          let s = Printf.sprintf "Лимит: %d" n in
          Utils.Option.iter (fun st ->
              st#root##.textContent := Js.some @@ Js.string s)
            subtitle) s in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Card.Markup.create ~outlined:true
    @@ List.map Widget.to_markup widgets in
  object

    inherit Widget.t elt () as super

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
    let signal, push = React.S.create
        ~eq:(Util_equal.Option.equal Stream.equal)
        None in
    let text = Stream.Source.to_string stream.source.info in
    let checkbox = Checkbox.make ~on_change:(fun w ->
        if w#checked then check.enable () else check.disable ();
        if w#checked then push @@ Some stream else push None;
        Lwt.return_unit)
        () in
    checkbox#toggle ~force:(Utils.Option.is_some url) ();
    let _s =
      React.S.map ~eq:(=)
        (fun v -> checkbox#set_disabled (not v)) check.avail in
    let item = Item_list.Item.make ~meta:checkbox text in
    item#add_class stream_class;
    if not present then item#add_class lost_class;
    item#set_on_destroy (fun () -> React.S.stop ~strong:true _s);
    item, signal

  let make_list state counter counter_push
      (stream_list : Stream.Table.stream list) =
    let open Stream.Table in
    let available = match state with
      | `Forbidden -> React.S.const false
      | `Unlimited -> React.S.const true
      | `Limited lim ->
        React.S.map ~eq:(=) (fun counter -> not (counter > lim)) counter in
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
      let eq = Util_equal.List.equal Stream.equal in
      React.S.merge ~eq:Util_equal.(List.equal (Option.equal Stream.equal))
        (fun acc v -> v :: acc) [] stream_signals
      |> React.S.map ~eq (Utils.List.filter_map (fun x -> x)) in
    let non_interactive = match state with
      | `Forbidden -> true | _ -> false in
    let list =
      Item_list.make
        ~non_interactive
        ~dense
        items in
    list, settings

  class t (state : Stream.Table.source_state)
      (list : Item_list.t)
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
        self#check_empty list#items

      method! destroy () : unit =
        super#destroy ();
        React.S.stop ~strong:true counter;
        React.S.stop ~strong:true settings;
        Utils.Option.iter (React.S.stop ~strong:true) left

      method private check_empty items : unit =
        match items with
        | [] ->
          body#remove_child list;
          body#append_child empty
        | _ ->
          body#remove_child empty;
          body#append_child list

    end

  let make_entry state counter counter_push left stream_list input =
    let list, settings = make_list state counter counter_push stream_list in
    let w = new t state list settings counter left (Topology.Input input) () in
    w#widget, settings

  let make (board : Topology.topo_board option)
      ((marker, state, stream_list) : Stream.stream_table_row) =
    let open Stream.Table in
    let open Topology in
    let inputs = match board with
      | None -> []
      | Some b -> Topology.get_inputs (`Boards [b]) in
    let counter, counter_push =
      let init_list =
        Utils.List.filter_map (function
            | ({ url = Some _; stream; _ } : stream) -> Some stream
            | { url = None;  _ } -> None) stream_list in
      React.S.create ~eq:(=) (List.length init_list) in
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
    let eq_l = Util_equal.List.equal Stream.equal in
    let eq = Util_equal.Pair.equal Stream.equal_marker eq_l in
    let settings =
      React.S.merge ~eq:eq_l (@) [] settings
      |> React.S.map ~eq (fun x -> marker, x) in
    w, settings

end

module Input = struct

  module Stream_dialog = struct

    let merge input addr port =
      let open Stream in
      match addr, port with
      | Some (addr : Netlib.Ipaddr.V4.t), Some (port : int) ->
        let source =
          { info = IPV4 { scheme = "udp"
                        ; addr
                        ; port }
          ; node = Entry (Input input) } in
        Ok { id = make_id source
           ; orig_id = TSoIP { scheme = "udp"; addr; port }
           ; typ = TS
           ; source
           }
      | _ -> Error "no data provided"

    let make (input : Topology.topo_input) =
      let open Stream in
      let ipv4 = Textfield.Custom { input_type = `Text
                                  ; to_string = Ipaddr.V4.to_string
                                  ; of_string = (fun x ->
                                        match Ipaddr.V4.of_string x with
                                        | Error `Msg s -> Error s
                                        | Ok _ as x -> x) } in
      let s_addr, push_addr = React.S.create None in
      let s_port, push_port = React.S.create None in
      let eq = Util_equal.Result.equal ~error:String.equal ~ok:equal in
      let result = React.S.l2 ~eq (merge input) s_addr s_port in
      let addr =
        Textfield.make_textfield
          ~label:"IP адрес"
          ~on_input:(fun _ i -> Lwt.return @@ push_addr i#value)
          ipv4 in
      let port =
        Textfield.make_textfield
          ~label:"UDP порт"
          ~on_input:(fun _ i -> Lwt.return @@ push_port i#value)
          (Integer ((Some 0), (Some 65535))) in
      let accept = Button.make ~label:"применить" () in
      let cancel = Button.make ~label:"отмена" () in
      accept#set_attribute Dialog.Attr.action @@ Dialog.action_to_string Accept;
      cancel#set_attribute Dialog.Attr.action @@ Dialog.action_to_string Close;
      let _s = React.S.map (function
          | Error _ -> accept#set_disabled true
          | Ok _ -> accept#set_disabled false) result in
      let box = Box.make ~dir:`Column [addr#widget; port#widget] in
      let title =
        Tyxml_js.To_dom.of_element
        @@ Dialog.Markup.create_title_simple ~title:"Добавление потока" () in
      let content =
        Tyxml_js.To_dom.of_element
        @@ Dialog.Markup.create_content ~content:[box#markup] () in
      let actions = [accept#root; cancel#root] in
      let dialog = Dialog.make ~title ~content ~actions () in
      dialog#add_class dialog_class;
      dialog#set_on_destroy (fun () ->
          React.S.stop ~strong:true s_addr;
          React.S.stop ~strong:true s_port);
      let show () = dialog#open_await () in
      { dialog; show; result }

    let show dialog streams =
      dialog.show ()
      >>= function
      | Close | Destroy | Custom _ -> Lwt.return_error "dialog was canceled"
      | Accept ->
        React.S.value dialog.result
        |> function
        | Error e -> Lwt.return_error e
        | Ok s ->
          if List.exists (Stream.equal s) (React.S.value streams)
          then Lwt.return_error "streams exists"
          else Lwt.return_ok s

  end

  let make_stream_list stream_list =
    let make_board_stream_entry del_item del_stream (stream : Stream.t) =
      let text = Stream.Source.to_string stream.source.info in
      let icon = Icon.SVG.(make_simple Path.delete)#root in
      let del_button = Icon_button.make
          ~ripple:false
          ~icon
          () in
      let item =
        Item_list.Item.make
          ~meta:del_button
          text in
      let click = Events.clicks del_button#root (fun _ _ ->
          del_item item;
          del_stream stream;
          Lwt.return_unit) in
      item#set_on_destroy (fun () -> Lwt.cancel click);
      item in
    let signal, push = React.S.create
        ~eq:(Util_equal.List.equal Stream.equal)
        stream_list in
    let list = Item_list.make
        ~dense [] in
    let del_item i =
      list#remove_child i;
      list#layout () in
    let del_stream s =
      let slst = React.S.value signal in
      push @@ List.filter (fun x -> not @@ Stream.equal s x) slst
    in
    let items = List.map (make_board_stream_entry del_item del_stream) stream_list in
    List.iter list#append_child items;
    list#layout ();
    let add stream =
      let slst = React.S.value signal in
      if List.exists (Stream.equal stream) slst
      then failwith "stream exists"; (* TODO fix *)
      let item = make_board_stream_entry del_item del_stream stream in
      list#append_child item;
      list#layout ();
      push (stream :: slst)
    in
    signal, list, add

  class t ((iid, state, stream_list) : Stream.stream_table_row) () =
    let open Stream.Table in
    let input, id = match iid with
      | `Input (inp, id) -> inp, id
      | _ -> failwith "impossible" in
    let (topo_input : Topology.topo_input) = { input; id } in
    let init_list =
      Utils.List.filter_map (function
          | ({ url = Some _; stream; _ } : stream) -> Some stream
          | { url = None;  _ } -> None)
        stream_list in
    let dialog = Stream_dialog.make topo_input in
    let streams, list, add = make_stream_list init_list in
    let settings =
      React.S.map ~eq:(fun (x1, s1) (x2, s2) ->
          let eq_x = match x1, x2 with
            | `Input (ip1, i1), `Input (ip2, i2) ->
              Topology.equal_input ip1 ip2 && i1 = i2
            | _ -> false in
          let eq_s = (Util_equal.List.equal Stream.equal) s1 s2 in
          eq_x && eq_s)
        (fun slst -> iid, slst) streams in
    let add = fun () ->
      Stream_dialog.show dialog streams >>= function
      | Error e -> Lwt.return @@ print_endline e
      | Ok s -> Lwt.return @@ add s in
    let apply = Button.make
        ~on_click:(fun _ _ _ -> add ())
        ~label:"Добавить поток" () in
    let buttons = Card.Actions.make_buttons [apply] in
    let body = Widget.create_div () in
    let empty = make_empty_placeholder () in
    object(self)
      val mutable _s = None
      inherit base ~state
          ~body
          ~actions:[buttons]
          ~entry:(Input topo_input)
          () as super

      method! init () : unit =
        super#init ();
        empty#add_class empty_placeholder_class;
        _s <- Some (React.S.map ~eq:(=) self#check_empty streams);
        Dom.appendChild Dom_html.document##.body dialog.dialog#root

      method! destroy () : unit =
        super#destroy ();
        Element.remove_child_safe Dom_html.document##.body dialog.dialog#root;
        React.S.stop ~strong:true settings;
        Utils.Option.iter (React.S.stop ~strong:true) _s;
        _s <- None

      method settings = settings

      method private check_empty items : unit =
        match items with
        | [] ->
          body#remove_child list;
          body#append_child empty
        | _ ->
          body#remove_child empty;
          body#append_child list

    end

  let make (row : Stream.stream_table_row) =
    let w = new t row () in
    [w#widget], w#settings

end

let make_entry : Topology.topo_cpu -> Stream.stream_table_row ->
  (Widget.t list) * ((Stream.marker * Stream.t list) React.signal) =
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

let make_table cpu (table : Stream.stream_table) =
  let widgets, signals = List.split @@ List.map (make_entry cpu) table in
  let widgets = List.concat widgets in
  let list = Box.make ~dir:`Column widgets in
  list#set_on_destroy (fun () ->
      List.iter (fun w -> w#destroy ()) widgets);
  let eq = Stream.equal_stream_setting in
  list,
  React.S.map
    ~eq:(Util_equal.Option.equal eq)
    (fun x -> Some x)
    (React.S.merge ~eq (fun acc v -> v :: acc) [] signals)

class t
    ~(init : Stream.stream_table)
    ~(event : Stream.stream_table React.event)
    (cpu : Topology.topo_cpu) =
  let id = "settings-place" in
  let post = Application_http_js.set_streams in
  let s_in = React.S.hold ~eq:Stream.equal_stream_table init event in
  let make (table : Stream.stream_table) =
    let dis, s = make_table cpu table in
    let place = dis in
    place#root##.id := Js.string id;
    place, s in
  let eq_w = Widget.equal in
  let eq_ss = React.S.equal ~eq:(Util_equal.Option.equal Stream.equal_stream_setting) in
  let s_div =
    React.S.map ~eq:(Util_equal.Pair.equal eq_w eq_ss) (fun s -> make s) s_in in
  let e_div =
    React.S.diff (fun _ (o, _) ->
        o#destroy ()) s_div in
  let div = Widget.create_div () in
  let s =
    let eq = Util_equal.Option.equal Stream.equal_stream_setting in
    React.S.switch ~eq
      (React.S.map ~eq:(React.S.equal ~eq)
         (fun n ->
           div#remove_children ();
           let w, n_s = n in
           div#append_child w;
           n_s) s_div) in
  let submit = Button.make
      ~label:"Применить"
      ~on_click:(fun w _ _ ->
          match React.S.value s with
          | None -> Lwt.return_unit
          | Some x ->
            let t = post x >>= fun _ -> Lwt.return_unit in
            w#set_loading_lwt t;
            t)
      () in
  let buttons = Card.Actions.make_buttons [submit] in
  let actions = Card.Actions.make [buttons] in
  object

    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      div#add_class inputs_class;
      super#add_class base_class;
      super#append_child div;
      super#append_child actions

    method! destroy () : unit =
      super#destroy ();
      submit#destroy ();
      actions#destroy ();
      buttons#destroy ();
      React.E.stop ~strong:true e_div;
      React.S.stop ~strong:true s;
      React.S.stop ~strong:true s_div;
      React.S.stop ~strong:true s_in

  end

let make ~init ~event cpu () =
  new t ~init ~event cpu
