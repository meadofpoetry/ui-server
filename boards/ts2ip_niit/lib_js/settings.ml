open Board_types
open Components

type state =
  { streams : WebSockets.webSocket Js.t
  ; config  : WebSockets.webSocket Js.t
  }

type init =
  { streams : Common.Stream.t list
  ; config  : config_response
  }

type events =
  { streams : Common.Stream.t list React.event
  ; config  : config_response React.event
  }

let clean_state (state : state) =
  state.streams##close; state.config##close

let streams ~(init   : init)
            ~(events : events) =
  let id            = "streams-list" in
  let div           = Dom_html.createDiv Dom_html.document in
  let s,s_push      = React.S.create [] in
  let _ = React.S.map (fun sms ->
              let items =
                CCList.map (fun x ->
                    let open Common.Stream in
                    let rec get_input = function
                      | Input x -> x
                      | Parent x -> get_input x.source
                    in
                    let input = get_input x.source in
                    let cb    = new Checkbox.t () in
                    let text  = Printf.sprintf "Источник: %s" (Common.Topology.input_to_string input.input) in
                    new Item_list.Item.t ~ripple:true ~start_detail:cb ~text (),
                    React.S.map (function true  -> Some x | false -> None) cb#s_state) sms
              in
              let lst = new Item_list.t ~items:(CCList.map (fun x -> `Item (fst x)) items) () in
              lst#set_id id;
              (try Dom.removeChild div (Dom_html.getElementById id) with _ -> ());
              Dom.appendChild div lst#root;
              let s = React.S.merge (fun acc x -> match x with
                                                  | Some x -> x :: acc
                                                  | None   -> acc) [] @@ CCList.map snd items
              in
              React.S.map (fun x -> s_push x) s |> ignore)
                      (React.S.hold init.streams events.streams)
  in
  Widget.create div,s

let card control
         ~(init   : init)
         ~(events : events) =
  let title   = new Card.Title.t ~title:"Настройки" () in
  let primary = new Card.Primary.t ~widgets:[title] () in
  let sms_lst,selected = streams ~init ~events in
  let apply   = new Button.t ~label:"Применить" () in
  let actions = new Card.Actions.t ~widgets:[apply] () in
  title#add_class "color--primary-on-primary";
  primary#add_class "background--primary";
  let card = new Card.t ~sections:[ `Primary primary
                                  ; `Media (new Card.Media.t ~widgets:[sms_lst] ())
                                  ; `Actions actions
                                  ] ()
  in
  React.E.map (fun _ -> Requests.post_streams_simple control (React.S.value selected)) apply#e_click |> ignore;
  card

let layout control
           ~(init   : init)
           ~(events : events) =
  let card = card control ~init ~events in
  let cell = new Layout_grid.Cell.t ~widgets:[card] () in
  cell#set_span 6;
  let grid = new Layout_grid.t ~cells:[cell] () in
  grid

let page control =
  let open Lwt_result.Infix in
  let div = Dom_html.createDiv Dom_html.document in
  let t   =
    Requests.get_config control
    >>= (fun config ->
      Requests.get_streams control
      >>= (fun streams ->
           let config_ws,config_sock   = Requests.get_config_ws control in
           let streams_ws,streams_sock = Requests.get_streams_ws control in
           Dom.appendChild div (layout control
                                       ~init:{ streams; config }
                                       ~events:{ streams = streams_ws; config = config_ws })#root;
           Lwt_result.return ({ streams = streams_sock
                              ; config  = config_sock } : state)))
  in
  div,(fun () -> t >>= (fun x -> clean_state x; Lwt_result.return ()) |> ignore)
