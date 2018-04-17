open Containers
open Components
open Lwt.Infix

open Application_types

type check = { avail   : bool React.signal
             ; enable  : unit -> unit
             ; disable : unit -> unit
             }

type stream_dialog = { dialog : Dialog.t
                     ; push   : (Common.Topology.input * int) -> unit
                     ; result : (Common.Stream.t, string) result React.signal
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
    | Some u -> new Item_list.Item.t ~text ~secondary_text:(Common.Url.to_string u) ~end_detail:checkbox ()
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
      new Item_list.Item.t ~text ~secondary_text:(Common.Url.to_string uri) ~end_detail:del_button () in
    (* TODO remove event *)
    Lwt_react.E.map (fun _ -> del_item item; del_stream stream) del_button#e_click |> ignore;
    item
  in
  let signal, push = React.S.create stream_list in
  let list  = new Item_list.t ~items:[] () in
  let del_item   i = list#remove_item i in
  let del_stream s =
    let slst = React.S.value signal in
    push @@ List.filter (fun x -> not @@ Common.Stream.equal s x) slst
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
  
let make_stream_create_dialog () =
  let open Common.Stream in
  let input, push_input = React.S.create (Input { input = RF; id = 0} ) in
  let push (input, id) = push_input (Input { input; id}) in

  let header = new Typography.Text.t ~text:"Create stream" () in
  let uri_box = new Textfield.t
                    ~input_id:"uri"
                    ~input_type:(Widget.Custom (Common.Url.of_string, Common.Url.to_string))
                    ~label:"Uri"
                    ()
  in
  let desc_box = new Textfield.t ~label:"description" ~input_id:"description" ~input_type:Widget.Text () in
  let box     = new Box.t ~vertical:true ~widgets:[uri_box#widget; desc_box#widget] () in
  
  let accept  = new Dialog.Action.t ~label:"accept" ~typ:`Accept () in
  let decline = new Dialog.Action.t ~label:"decline" ~typ:`Decline () in
  let dialog  = new Dialog.t ~actions:[accept; decline] ~content:(`Widgets [header#widget; box#widget]) () in

  let merge uri description source =
    match uri with
    | None -> Error ("no uri provided")
    | Some uri -> Ok { id = `Ip uri; description; source }
  in
  let result = React.S.l3 merge uri_box#s_input desc_box#s_input input in
  Dom.appendChild Dom_html.document##.body dialog#root;
  { dialog; push; result }

let show_stream_create_dialog dialog streams i =
  let open Common.Topology in
  dialog.push i;
  dialog.dialog#show_await >>= function
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
  Lwt_react.E.keep @@ Lwt_react.E.map_p (fun _ ->
                          show_stream_create_dialog dialog streams (input,id) >>= function
                          | Error e -> Lwt.return @@ print_endline e
                          | Ok s    -> Lwt.return @@ add s)
                                        add_button#e_click;
  let box = new Box.t ~vertical:true ~widgets:[subheader#widget; list#widget; add_button#widget] () in
  box#widget, settings
  
let make_entry : 'a -> Widget.widget * (marker * Common.Stream.t list) React.signal = function
  | `Input _, _, _ as x -> make_input_entry x
  | `Board _, _, _ as x -> make_board_entry x
                         
let make_table table =
  let widgets, signals = List.split @@ List.map make_entry table in
  let list  = new Box.t ~vertical:true ~widgets () in
  list, (React.S.merge ~eq:Equal.physical (fun acc v -> v::acc) [] signals)
  
let make ~(init:stream_table)
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

class t () = object(self)

  val mutable _sock = None
  inherit Widget.widget (Dom_html.createDiv Dom_html.document) ()

  method init =
    Requests.get_stream_table ()
    >>= (function
         | Ok init ->
            let events,sock = Requests.get_stream_table_socket () in
            let stream_selector =
              make ~init ~events
                   ~post:(fun ss ->
                     (Requests.post_stream_settings ss
                      >>= function
                      | Ok ()   -> Lwt.return_unit
                      | Error e -> (Printf.printf "post stream settings, error: %s\n"
                                                  (Yojson.Safe.pretty_to_string @@
                                                     Application_types.set_error_to_yojson e));
                                   Lwt.return_unit)
                     |> Lwt.ignore_result)
            in
            _sock <- Some sock;
            Dom.appendChild self#root stream_selector;
            Lwt.return_unit
         | Error e -> Lwt.return @@ Printf.printf "stream_table get, error: %s\n" e)
    |> Lwt.ignore_result

  method destroy = Option.iter (fun x -> x##close) _sock

  initializer
    self#init

end

let create () = new t ()
