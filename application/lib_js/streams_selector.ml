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

class base ?actions ~body ?(lim : int React.signal option)
        ~(inputs : Topology.topo_input list) () =

  let title =
    let s = List.map Topology.get_input_name inputs in
    match s with
    | [] -> "Неизвестный источник"
    | [s] -> "Вход " ^ s
    | l -> "Входы " ^ (String.concat ", " l) in
  let title = new Card.Primary.title title () in
  let subtitle = match lim with
    | None -> None
    | Some x -> Some (new Card.Primary.subtitle "" ()) in
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
  let s = match lim with
    | None -> React.S.const ()
    | Some s ->
       React.S.map ~eq:Equal.unit (fun n ->
           let s = Printf.sprintf "Доступно для выбора: %d" n in
           Option.iter (fun st -> st#set_text_content s) subtitle) s in
  object

    inherit Card.t
              ~outlined:true
              ~widgets
              () as super

    method init () : unit =
      super#init ();
      super#add_class block_class

    method destroy () : unit =
      super#destroy ();
      React.S.stop ~strong:true s

  end

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
    let item =
      new Item_list.Item.t
        ~text
        ~meta:checkbox
        ~value:()
        () in
    let s =
      React.S.map ~eq:(Equal.option Stream.equal)
        (fun s -> if s then Some stream else None)
        checkbox#s_state in
    item, s

  module Limited = struct

    let make_list lim marker (stream_list : Stream.Table.stream list) =
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
        |> React.S.map ~eq (fun l -> (marker, List.filter_map Fun.id l)) in
      let list =
        new Item_list.t
          ~dense:true
          ~items:(List.map (fun i -> `Item i) items)
          () in
      list, settings, counter

    class t (lim : int)
            (marker : marker)
            (board : Topology.topo_board option)
            (stream_list : Stream.Table.stream list) () =
      let open Item_list.List_group in
      let open Stream.Table in
      let inputs =
        Option.map_or ~default:[]
          (fun b -> Topology.get_inputs (`Boards [b])) board in
      let list, settings, counter = make_list lim marker stream_list in
      object

        inherit base ~lim:(React.S.map ~eq:(=) (fun x -> lim - x) counter)
                  ~body:list
                  ~inputs
                  () as super

        method destroy () : unit =
          super#destroy ();
          React.S.stop ~strong:true counter;
          React.S.stop ~strong:true settings

        method settings = settings

      end

    let make lim marker board streams =
      let w = new t lim marker board streams () in
      w#widget, w#settings

  end

  module Unlimited = struct

    let make marker board (stream_list : Stream.Table.stream list) =
      let open Item_list.List_group in
      let open Stream.Table in
      let items, stream_signals =
        List.split
        @@ List.map (fun ({ url; stream; _ } : stream) ->
               make_stream_entry ~url stream)
             stream_list in
      let subheader = new Typography.Text.t ~text:(Printf.sprintf "Board") () in
      let list =
        new Item_list.t
          ~dense:true
          ~items:(List.map (fun i -> `Item i) items)
          () in
      let settings =
        let eq = Equal.pair equal_marker (Equal.list Stream.equal) in
        React.S.merge ~eq:(Equal.list (Equal.option Stream.equal))
          (fun acc v -> v :: acc) [] stream_signals
        |> React.S.map ~eq (fun l -> (marker, List.filter_map Fun.id l))
      in
      let box  = new Vbox.t ~widgets:[subheader#widget; list#widget] () in
      box#widget, settings

  end

  module Forbidden = struct

    class t (marker : marker)
            (board: Topology.topo_board option)
            (stream_list : Stream.Table.stream list) () =
      let open Item_list.List_group in
      let open Stream.Table in
      let init_list =
        List.filter_map (function
            | ({ url = Some _; stream; _ } : stream) -> Some stream
            | { url = None;  _} -> None) stream_list in
      let inputs =
        Option.map_or ~default:[]
          (fun b -> Topology.get_inputs (`Boards [b])) board in
      let settings = React.S.const (marker, init_list) in
      let list = new Item_list.t ~items:[] () in
      object

        inherit base ~body:list ~inputs ()

        method settings = settings

      end

    let make marker board streams =
      let w = new t marker board streams () in
      w#widget, w#settings

  end

  let make (board : Topology.topo_board option)
        ((marker, state, stream_list) : stream_table_row) =
    match state with
    | `Forbidden -> Forbidden.make marker board stream_list
    | `Limited lim -> Limited.make lim marker board stream_list
    | `Unlimited -> Unlimited.make marker board stream_list

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
    object(self)

      inherit base ~body:list
                ~actions:[buttons]
                ~inputs:[topo_input]
                () as super

      method init () : unit =
        super#init ();
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

let make_entry : Topology.topo_cpu -> stream_table_row ->
                 Widget.t * (marker * Stream.t list) React.signal =
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
  let list = new Vbox.t ~widgets () in
  list#set_on_destroy
  @@ Some (fun () ->
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

let make ~init ~event cpu () =
  new t ~init ~event cpu ()
