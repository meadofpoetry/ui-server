open Containers
open Components
open Lwt.Infix
open Application_types
open Common

let base_class = "application-stream-selector"
let block_class = Markup.CSS.add_element base_class "block"
let dialog_class = Markup.CSS.add_element base_class "dialog"

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

module Board = struct

  let make_stream_entry ?(check = None)
        ?(url = None)
        (stream : Stream.t) =
    let text =
      let i = Stream.get_input stream in
      let s = Stream.Source.to_string stream.source.info in
      match i with
      | Some i -> Printf.sprintf "Вход %s: %s" (Topology.get_input_name i) s
      | None -> s in
    let checkbox = new Checkbox.t ~ripple:false () in
    checkbox#set_checked @@ Option.is_some url;
    begin match check with
    | None -> ()
    | Some check ->
       React.S.map ~eq:Equal.unit
         (fun v -> checkbox#set_disabled (not v)) check.avail
       |> React.S.keep;
       React.S.diff (fun s _ ->
           if s then check.enable () else check.disable ()) checkbox#s_state
       |> React.E.keep;
    end;
    let item = match url with
      | None ->
         new Item_list.Item.t
           ~text
           ~meta:checkbox
           ~value:()
           ()
      | Some u ->
         new Item_list.Item.t
           ~text
           ~secondary_text:(Url.to_string u)
           ~meta:checkbox ~value:() () in
    let s =
      React.S.map ~eq:(Equal.option Stream.equal)
        (fun s -> if s then Some stream else None)
        checkbox#s_state in
    item, s

  module Limited = struct

    let make_list lim bid (stream_list : Stream.Table.stream list) =
      let open Stream.Table in
      let init_list =
        List.filter_map (function
            | ({ url = Some _; stream; _ } : stream) -> Some stream
            | { url = None;  _} -> None) stream_list in
      let counter, counter_push =
        React.S.create ~eq:Int.equal (List.length init_list) in
      let check =
        { avail = React.S.map ~eq:Bool.equal
                    (fun counter -> not (counter > lim)) counter
        ; enable = (fun () -> counter_push @@ succ @@ React.S.value counter)
        ; disable = (fun () -> counter_push @@ pred @@ React.S.value counter)
        } in
      let items, stream_signals =
        List.split
        @@ List.map (fun ({ url; stream; _ } : stream) ->
               make_stream_entry ~check:(Some check) ~url stream)
             stream_list in
      let settings =
        let eq = Equal.pair equal_marker (Equal.list Stream.equal) in
        React.S.merge ~eq:(Equal.list (Equal.option Stream.equal))
          (fun acc v -> v :: acc) [] stream_signals
        |> React.S.map ~eq (fun l -> (bid, List.filter_map Fun.id l)) in
      let list =
        new Item_list.t
          ~items:(List.map (fun i -> `Item i) items)
          () in
      list, settings, counter

    class t (lim : int)
            (bid : marker)
            (stream_list : Stream.Table.stream list) () =
      let open Item_list.List_group in
      let open Stream.Table in
      let id = match bid with
        | `Board id -> id
        | _ -> failwith "impossible" in
      let list, settings, counter = make_list lim bid stream_list in
      let title = new Card.Primary.subtitle "" () in
      let primary = new Card.Primary.t ~widgets:[title] () in
      let media = new Card.Media.t ~widgets:[list] () in
      let s =
        React.S.map ~eq:Equal.unit (fun counter ->
            let str = Printf.sprintf "Board: %d, streams left: %d"
                        id (lim - counter) in
          title#set_text_content str) counter in
      object

        inherit Card.t
                  ~outlined:true
                  ~widgets:[ primary#widget
                           ; media#widget ] () as super

        method init () : unit =
          super#init ();
          super#add_class block_class;

        method destroy () : unit =
          super#destroy ();
          React.S.stop ~strong:true s;
          React.S.stop ~strong:true counter;
          React.S.stop ~strong:true settings

        method settings = settings

      end

    let make lim bid streams =
      let w = new t lim bid streams () in
      w#widget, w#settings

  end

  module Unlimited = struct

    let make bid (stream_list : Stream.Table.stream list) =
      let open Item_list.List_group in
      let open Stream.Table in
      let id = match bid with
        | `Board id -> id
        | _ -> failwith "impossible" in
      let items, stream_signals =
        List.split
        @@ List.map (fun ({ url; stream; _ } : stream) ->
               make_stream_entry ~url stream)
             stream_list in
      let subheader = new Typography.Text.t ~text:(Printf.sprintf "Board: %d" id) () in
      let list = new Item_list.t ~items:(List.map (fun i -> `Item i) items) () in
      let settings =
        let eq = Equal.pair equal_marker (Equal.list Stream.equal) in
        React.S.merge ~eq:(Equal.list (Equal.option Stream.equal))
          (fun acc v -> v :: acc) [] stream_signals
        |> React.S.map ~eq (fun l -> (bid, List.filter_map Fun.id l))
      in
      let box  = new Vbox.t ~widgets:[subheader#widget; list#widget] () in
      box#widget, settings

  end

  module Forbidden = struct

    class t (bid : marker)
            (stream_list : Stream.Table.stream list) () =
      let open Item_list.List_group in
      let open Stream.Table in
      let init_list =
        List.filter_map (function
            | ({ url = Some _; stream; _ } : stream) -> Some stream
            | { url = None;  _} -> None) stream_list in
      let settings  = React.S.const (bid, init_list) in
      let id = match bid with
        | `Board id -> id
        | _ -> failwith "impossible" in
      let title = new Card.Primary.title (Printf.sprintf "Board: %d" id) () in
      let primary = new Card.Primary.t ~widgets:[title] () in
      let list = new Item_list.t ~items:[] () in
      object

        inherit Card.t
                  ~outlined:true
                  ~widgets:[primary#widget; list#widget]
                  () as super

        method init () : unit =
          super#init ();
          super#add_class block_class;

        method destroy () : unit =
          super#destroy ()

        method settings = settings

      end

    let make bid streams =
      let w = new t bid streams () in
      w#widget, w#settings

  end

  let make ((bid, state, stream_list) : stream_table_row) =
    match state with
    | `Forbidden -> Forbidden.make bid stream_list
    | `Limited lim -> Limited.make lim bid stream_list
    | `Unlimited -> Unlimited.make bid stream_list

end

module Input = struct

  module Stream_dialog = struct

    let merge input uri =
      let open Stream in
      match uri with
      | None -> Error ("no uri provided")
      | Some (uri : Url.t) ->
         let source =
           { info = IPV4 { scheme = "udp"
                         ; addr = uri.ip
                         ; port = uri.port }
           ; node = Entry (Input input) } in
         Ok { id = make_id source
            ; orig_id = TSoIP { addr = uri.ip; port = uri.port }
            ; typ = TS
            ; source
           }

    let make (input : Topology.topo_input) =
      let open Stream in
      let uri =
        new Textfield.t
          ~input_type:(Custom (Url.of_string, Url.to_string))
          ~label:"URL"
          () in
      let eq = Result.equal ~err:String.equal equal in
      let result = React.S.map ~eq (merge input) uri#s_input in
      let accept =
        new Ui_templates.Buttons.Set.t ~label:"применить"
          (React.S.map ~eq:(Equal.option equal)
             Result.to_opt result)
          (fun _ -> Lwt.return_unit)
          () in
      let cancel = new Button.t ~label:"отмена" () in
      let dialog =
        new Dialog.t
          ~title:"Добавление потока"
          ~actions:[ Dialog.Action.make ~typ:`Accept accept
                   ; Dialog.Action.make ~typ:`Cancel cancel ]
          ~content:(`Widgets [uri#widget])
          () in
      dialog#add_class dialog_class;
      let show () =
        let t = dialog#show_await () in
        uri#focus ();
        t in
      { dialog; show; result }

    let show dialog streams =
      let open Topology in
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
      let text =
        Printf.sprintf "Поток %s"
        @@ Stream.Source.to_string stream.source.info in
      let icon = Icon.SVG.(create_simple Path.delete) in
      let del_button = new Icon_button.t ~icon () in
      let uri = match stream.orig_id with
        | TSoIP x -> ({ ip = x.addr; port = x.port } : Url.t) in
      let item =
        new Item_list.Item.t
          ~text
          ~secondary_text:(Url.to_string uri)
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
        ~dense:true
        ~two_line:true
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

  class t ((iid, _, stream_list) : stream_table_row) () =
    let open Item_list.List_group in
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
    let text =
      Printf.sprintf "Вход %s"
      @@ Topology.get_input_name { input; id} in
    let dialog = Stream_dialog.make topo_input in
    let title = new Card.Primary.subtitle text () in
    let primary = new Card.Primary.t ~widgets:[title] () in
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
    let media = new Card.Media.t ~widgets:[list] () in
    let apply =
      new Ui_templates.Buttons.Set.t
        ~label:"Добавить поток"
        (React.S.const @@ Some ())
        add
        () in
    let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
    let actions = new Card.Actions.t ~widgets:[buttons] () in
    object(self)

      inherit Card.t
                ~outlined:true
                ~widgets:[ primary#widget
                         ; media#widget
                         ; (new Divider.t ())#widget
                         ; actions#widget ]
                () as super

      method init () : unit =
        super#init ();
        super#add_class block_class;
        Dom.appendChild Dom_html.document##.body dialog.dialog#root

      method destroy () : unit =
        super#destroy ();
        Dom.removeChild Dom_html.document##.body dialog.dialog#root;
        React.S.stop ~strong:true settings

      method settings = settings

    end

  let make (row : stream_table_row) =
    let w = new t row () in
    w#widget, w#settings

end

let make_entry : stream_table_row ->
                 Widget.t * (marker * Stream.t list) React.signal = function
  | `Input _, _, _ as x -> Input.make x
  | `Board _, _, _ as x -> Board.make x

let make_table (table : stream_table) =
  let widgets, signals = List.split @@ List.map make_entry table in
  let list = new Vbox.t ~widgets () in
  list#set_on_destroy
  @@ Some (fun () ->
         List.iter (fun w -> w#destroy ()) widgets);
  list, React.S.map ~eq:(Equal.option equal_stream_setting) Option.return
          (React.S.merge ~eq:equal_stream_setting (fun acc v -> v :: acc)
             [] signals)

class t ~(init : stream_table)
        ~(event : stream_table React.event)
        () =
  let id = "settings-place" in
  let post = Requests.HTTP.set_streams in
  let s_in = React.S.hold ~eq:equal_stream_table init event in
  let make (table : stream_table) =
    let dis, s = make_table table in
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
           Dom.appendChild div#root w#root;
           n_s) s_div) in
  let apply = new Ui_templates.Buttons.Set.t s post () in
  let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
  let actions = new Card.Actions.t ~widgets:[buttons] () in
  object(self)

    inherit Vbox.t ~widgets:[div; actions#widget] () as super

    method init () : unit =
      super#init ();
      self#add_class base_class;

    method destroy () : unit =
      super#destroy ();
      apply#destroy ();
      actions#destroy ();
      buttons#destroy ();
      React.E.stop ~strong:true e_div;
      React.S.stop ~strong:true s;
      React.S.stop ~strong:true s_div;
      React.S.stop ~strong:true s_in

  end

let make ~init ~event () =
  new t ~init ~event ()
