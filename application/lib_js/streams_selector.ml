open Containers
open Components

open Application_types

module Streams_table = struct

  type check = { avail   : bool React.signal
               ; enable  : unit -> unit
               ; disable : unit -> unit
               }
  
  let make_board_stream_entry ?(check = None)
        ?(uri = None)
        stream =
    let text           = Common.Stream.header stream in
    let checkbox       = new Checkbox.t ~ripple:false () in
    checkbox#set_checked @@ Option.is_some uri;
    begin match check with
    | None -> ()
    | Some check ->
       Lwt_react.S.keep @@ React.S.map (fun v -> checkbox#set_disabled (not v)) check.avail;
       Lwt_react.E.keep @@ React.S.diff (fun s _ -> if s then check.enable () else check.disable ()) checkbox#s_state;
    end;
    let item = match uri with
      | None   -> new Item_list.Item.t ~text ~end_detail:checkbox ()
      | Some u -> new Item_list.Item.t ~text ~secondary_text:(Common.Uri.to_string u) ~end_detail:checkbox ()
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
    let box  = new Box.t ~vertical:true ~widgets:[subheader#widget; list#widget] () in
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
    let box  = new Box.t ~vertical:true ~widgets:[subheader#widget; list#widget] () in
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
    let box  = new Box.t ~vertical:true ~widgets:[subheader#widget; list#widget] () in
    box#widget, settings
  
  let make_board_entry (bid, state, stream_list) =
    match state with
    | `Forbidden   -> make_board_forbidden bid stream_list
    | `Limited lim -> make_board_limited lim bid stream_list
    | `Unlimited   -> make_board_unlimited bid stream_list

  let make_input_stream_list stream_list =
    let make_board_stream_entry del_item del_stream stream =
      let text           = Common.Stream.header stream in
      let del_button     = new Button.t ~label:"delete" () in
      let uri            = match stream.id with `Ip u -> u in
      let item           =
        new Item_list.Item.t ~text ~secondary_text:(Common.Uri.to_string uri) ~end_detail:del_button () in
      (* TODO remove event *)
      Lwt_react.E.map (fun _ -> del_item item; del_stream stream) del_button#e_click |> ignore;
      item
    in
    let signal, push = React.S.create stream_list in
    let list  = new Item_list.t ~items:[] () in
    let del_item   i = list#remove_item i in
    let del_stream s =
      let slst = React.S.value signal in
      push @@ List.filter (Common.Stream.equal s) slst
    in
    let items = List.map (make_board_stream_entry del_item del_stream) stream_list in
    List.iter (fun i -> list#add_item i) items;
    let add stream =
      let slst = React.S.value signal in
      if List.exists (Common.Stream.equal stream) slst
      then failwith "stream exists"; (* TODO fix *)
      let item = make_board_stream_entry del_item del_stream stream in
      list#add_item item;
      push (stream::slst)
    in
    signal, list, add
                    
  let make_input_entry (iid, _, stream_list) =
    let open Item_list.List_group in
    let uri = ref (Result.get_exn @@ Common.Uri.of_string "udp://224.1.2.2:1234") in
     let input, id = match iid with
      | `Input (inp,id) -> inp,id
      | _ -> failwith "impossible"
    in
    let make_stream () : Common.Stream.t =
      uri := Common.Uri.succ !uri;
      { id = `Ip !uri; description = None; source = Input { input; id } }
    in
    let init_list = List.filter_map (function (Some _, s) -> Some s | (None, _) -> None) stream_list in
    let subheader = new Typography.Text.t
                      ~text:(Printf.sprintf "Input: %s (%d)" (Common.Topology.input_to_string input) id) () in
    let streams, list, add = make_input_stream_list init_list in
    let settings = React.S.map (fun slst -> iid, slst) streams in
    let add_button  = new Button.t ~label:"add stream" () in
    Lwt_react.E.keep @@ React.E.map (fun _ -> add (make_stream ())) add_button#e_click;
    let box = new Box.t ~vertical:true ~widgets:[subheader#widget; list#widget; add_button#widget] () in
    box#widget, settings
                    
  let make_entry : 'a -> Widget.widget * (marker * Common.Stream.t list) React.signal = function
    | `Input _, _, _ as x -> make_input_entry x
    | `Board _, _, _ as x -> make_board_entry x
  
  let make_table table =
    let widgets, signals = List.split @@ List.map make_entry table in
    let list  = new Box.t ~vertical:true ~widgets () in
    list, (React.S.merge ~eq:Equal.physical (fun acc v -> v::acc) [] signals)
  
  let create ~(init:stream_table)
        ~(events:stream_table React.event)
        ~(post:stream_setting -> unit) =
    let id  = "settings-place" in
    let div = Dom_html.createDiv Dom_html.document in
    let make (table : stream_table) =
      let dis, s = make_table table in
      let but    = new Components.Button.t ~label:"Применить" () in
      let place  = new Components.Card.t
                       ~widgets:[ dis#widget
                                ; (new Card.Actions.t ~widgets:[but] ())#widget ]
                       () in
      place#set_id id;
      but#button_element##.onclick := Dom.handler (fun _ -> post @@ React.S.value s; Js._false);
      place
    in
    Lwt_react.E.keep @@ React.E.map (fun st ->
                            (try Dom.removeChild div (Dom_html.getElementById id)
                             with _ -> print_endline "No el");
                            Dom.appendChild div (make st)#root)
                          events;
    Dom.appendChild div (make init)#root;
    div
end
