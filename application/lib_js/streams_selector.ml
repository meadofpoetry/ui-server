open Containers
open Components
open Lwt.Infix

open Application_types

module Topology = Common.Topology
module Stream = Common.Stream

type check = { avail   : bool React.signal
             ; enable  : unit -> unit
             ; disable : unit -> unit
             }

type stream_dialog =
  { dialog : Dialog.t
  ; push   : (Common.Topology.input * int) -> unit
  ; result : (Stream.t, string) result React.signal
  }
                   
let make_board_stream_entry ?(check = None)
                            ?(uri = None)
                            (stream:Stream.t) =
  let text =
    let i = Stream.get_input stream in
    let s = Stream.Source.to_string stream.source.info in
    match i with
    | Some i -> Printf.sprintf "Вход %s: %s" (Topology.get_input_name i) s
    | None   -> s in
  let checkbox = new Checkbox.t ~ripple:false () in
  checkbox#set_checked @@ Option.is_some uri;
  begin match check with
  | None -> ()
  | Some check ->
     Lwt_react.S.keep @@ React.S.map (fun v -> checkbox#set_disabled (not v)) check.avail;
     Lwt_react.E.keep @@ React.S.diff (fun s _ -> if s then check.enable () else check.disable ()) checkbox#s_state;
  end;
  let item = match uri with
    | None   -> new Item_list.Item.t
                  ~text
                  ~meta:checkbox
                  ~value:() ()
    | Some u -> new Item_list.Item.t
                  ~text
                  ~secondary_text:(Common.Url.to_string u)
                  ~meta:checkbox ~value:() ()
  in
  item, React.S.map (fun s -> if s then Some stream else None) checkbox#s_state
  
let make_board_limited lim bid stream_list =
  let open Item_list.List_group in
  let init_list = List.filter_map (function (Some _, s) -> Some s | (None, _) -> None) stream_list in
  let counter, counter_push = React.S.create (List.length init_list) in
  let check = { avail = React.S.map (fun counter -> not (counter > lim)) counter 
              ; enable = (fun () -> counter_push @@ succ @@ React.S.value counter)
              ; disable = (fun () -> counter_push @@ pred @@ React.S.value counter)
              }
  in
  let id = match bid with
    | `Board id -> id
    | _ -> failwith "impossible"
  in
  let items, stream_signals = List.split @@ List.map (fun (uri, stream) ->
                                                make_board_stream_entry ~check:(Some check) ~uri stream
                                              ) stream_list
  in
  let subheader = new Typography.Text.t ~text:"" () in
  let list = new Item_list.t ~items:(List.map (fun i -> `Item i) items) () in
  let settings =
    React.S.merge ~eq:Equal.physical (fun acc v -> v::acc) [] stream_signals
    |> React.S.map (fun l -> (bid, List.filter_map Fun.id l))
  in
  Lwt_react.S.keep @@
    React.S.map (fun counter -> let str = Printf.sprintf "Board: %d, streams left: %d" id (lim - counter) in
                                subheader#set_text str) counter;
  let box  = new Vbox.t ~widgets:[subheader#widget; list#widget] () in
  box#widget, settings

let make_board_unlimited bid stream_list =
  let open Item_list.List_group in
  let id = match bid with
    | `Board id -> id
    | _ -> failwith "impossible"
  in
  let items, stream_signals = List.split @@ List.map (fun (uri, stream) ->
                                                make_board_stream_entry ~uri stream
                                              ) stream_list
  in
  let subheader = new Typography.Text.t ~text:(Printf.sprintf "Board: %d" id) () in
  let list = new Item_list.t ~items:(List.map (fun i -> `Item i) items) () in
  let settings =
    React.S.merge ~eq:Equal.physical (fun acc v -> v::acc) [] stream_signals
    |> React.S.map (fun l -> (bid, List.filter_map Fun.id l))
  in
  let box  = new Vbox.t ~widgets:[subheader#widget; list#widget] () in
  box#widget, settings

let make_board_forbidden bid stream_list = 
  let open Item_list.List_group in
  let init_list = List.filter_map (function (Some _, s) -> Some s | (None, _) -> None) stream_list in
  let settings  = React.S.const (bid, init_list) in
  let id = match bid with
    | `Board id -> id
    | _ -> failwith "impossible"
  in
  let subheader = new Typography.Text.t ~text:(Printf.sprintf "Board: %d" id) () in
  let list = new Item_list.t ~items:[] () in
  let box  = new Vbox.t ~widgets:[subheader#widget; list#widget] () in
  box#widget, settings
  
let make_board_entry (bid, state, stream_list) =
  match state with
  | `Forbidden   -> make_board_forbidden bid stream_list
  | `Limited lim -> make_board_limited lim bid stream_list
  | `Unlimited   -> make_board_unlimited bid stream_list

let make_input_stream_list stream_list =
  let make_board_stream_entry del_item del_stream (stream:Stream.t) =
    let text           = Stream.Source.to_string stream.source.info in
    let del_button     = new Button.t ~label:"delete" () in
    let uri            = match stream.orig_id with
      | TSoIP x -> ({ ip = x.addr; port = x.port } : Common.Url.t) in
    let item           =
      new Item_list.Item.t ~text
        ~secondary_text:(Common.Url.to_string uri)
        ~meta:del_button
        ~value:() () in
    del_button#listen_click_lwt (fun _ _ ->
        del_item item; del_stream stream; Lwt.return_unit) |> Lwt.ignore_result;
    item
  in
  let signal, push = React.S.create stream_list in
  let list  = new Item_list.t ~items:[] () in
  let del_item   i = list#remove_item i in
  let del_stream s =
    let slst = React.S.value signal in
    push @@ List.filter (fun x -> not @@ Stream.equal s x) slst
  in
  let items = List.map (make_board_stream_entry del_item del_stream) stream_list in
  List.iter (fun i -> list#append_item i) items;
  let add stream =
    let slst = React.S.value signal in
    if List.exists (Stream.equal stream) slst
    then failwith "stream exists"; (* TODO fix *)
    let item = make_board_stream_entry del_item del_stream stream in
    list#append_item item;
    push (stream::slst)
  in
  signal, list, add
  
let make_stream_create_dialog () =
  let open Stream in
  let input, push_input = React.S.create ({ input = RF; id = 0} : Common.Topology.topo_input) in
  let push (input, id) = push_input ({ input; id}) in

  let header = new Typography.Text.t ~text:"Create stream" () in
  let uri_box = new Textfield.t
                    ~input_id:"uri"
                    ~input_type:(Custom (Common.Url.of_string, Common.Url.to_string))
                    ~label:"Uri"
                    ()
  in
  let desc_box =
    new Textfield.t
      ~label:"description"
      ~input_id:"description"
      ~input_type:Text
      () in
  let box     = new Vbox.t ~widgets:[uri_box#widget; desc_box#widget] () in
  
  let accept  = new Dialog.Action.t ~label:"accept" ~typ:`Accept () in
  let decline = new Dialog.Action.t ~label:"decline" ~typ:`Cancel () in
  let dialog  = new Dialog.t ~actions:[accept; decline] ~content:(`Widgets [header#widget; box#widget]) () in

  let merge uri description source =
    match uri with
    | None -> Error ("no uri provided")
    | Some (uri:Common.Url.t) ->
       let source =
         { info = IPV4 { scheme = "udp"
                       ; addr   = uri.ip
                       ; port   = uri.port }
         ; node = Entry (Input source) } in
       Ok { id      = make_id source
          ; orig_id = TSoIP { addr = uri.ip; port = uri.port }
          ; typ     = TS
          ; source
         }
  in
  let result = React.S.l3 merge uri_box#s_input desc_box#s_input input in
  Dom.appendChild Dom_html.document##.body dialog#root;
  { dialog; push; result }

let show_stream_create_dialog dialog streams i =
  let open Common.Topology in
  dialog.push i;
  dialog.dialog#show_await () >>= function
  | `Cancel -> Lwt.return_error "dialog was canceled"
  | `Accept ->
     React.S.value dialog.result
     |> Result.flat_map (fun s -> if List.exists (Common.Stream.equal s) (React.S.value streams)
                                  then Error "streams exists" else Ok s)
     |> Lwt.return
    
let make_input_entry (iid, _, stream_list) =
  let open Item_list.List_group in
  let input, id = match iid with
    | `Input (inp,id) -> inp,id
    | _ -> failwith "impossible"
  in
  let init_list = List.filter_map (function (Some _, s) -> Some s | (None, _) -> None) stream_list in
  let subheader = new Typography.Text.t
                      ~text:(Printf.sprintf "Input: %s (%d)" (Common.Topology.input_to_string input) id) () in
  let streams, list, add = make_input_stream_list init_list in
  let settings = React.S.map (fun slst -> iid, slst) streams in
  let add_button  = new Button.t ~label:"add stream" () in
  let dialog = make_stream_create_dialog () in
  add_button#listen_click_lwt (fun _ _ ->
      show_stream_create_dialog dialog streams (input, id) >>= function
      | Error e -> Lwt.return @@ print_endline e
      | Ok s -> Lwt.return @@ add s) |> Lwt.ignore_result;
  let box = new Vbox.t ~widgets:[subheader#widget; list#widget; add_button#widget] () in
  box#widget, settings

let make_entry : 'a -> Widget.t * (marker * Common.Stream.t list) React.signal = function
  | `Input _, _, _ as x -> make_input_entry x
  | `Board _, _, _ as x -> make_board_entry x

let make_table table =
  let widgets, signals = List.split @@ List.map make_entry table in
  let list  = new Vbox.t ~widgets () in
  list, React.S.map Option.return (React.S.merge ~eq:Equal.physical (fun acc v -> v::acc) [] signals)

let make ~(init:  stream_table)
         ~(event: stream_table React.event)
         () =
  let id  = "settings-place" in
  let div = Dom_html.createDiv Dom_html.document |> Widget.create in
  let make (table : stream_table) =
    let dis, s = make_table table in
    let place  = dis in
    place#set_id id;
    place,s
  in
  let s_in  = React.S.hold ~eq:equal_stream_table init event in
  let s_div = React.S.map ~eq:Equal.physical (fun s -> make s) s_in in
  let s     = React.S.switch ~eq:(Equal.option equal_stream_setting)
                             (React.S.map ~eq:(Equal.physical) (fun n ->
                                            div#set_empty ();
                                            let w,n_s = n in
                                            Dom.appendChild div#root w#root;
                                            n_s) s_div)
  in
  let post = Requests.HTTP.set_streams in
  div,s,post
