open Containers
open Board_types
open Components
open Common

type state =
  { streams : WebSockets.webSocket Js.t
  ; config  : WebSockets.webSocket Js.t
  }

type init =
  { streams : Common.Stream.t list
  ; config  : packer_settings list
  }

type events =
  { streams : Common.Stream.t list React.event
  ; config  : packer_settings list React.event
  }

let clean_state (state : state) =
  state.streams##close; state.config##close

let streams ~(init   : init)
      ~(events : events) =
  let id            = "streams-list" in
  let div           = Dom_html.createDiv Dom_html.document in
  let s,s_push      = React.S.create [] in
  let _ =
    React.S.map (fun sms ->
        let items =
          List.map (fun x ->
              let open Stream in
              let open Topology in
              let rec get_input = function
                | Entry (Input x) -> "Вход " ^ (Topology.get_input_name x)
                | Entry (Board b) -> Printf.sprintf "Плата %s" b.model
                | Stream x        -> get_input x.source.node in
              let info = Stream.Source.to_string x.source.info in
              let text =
                Printf.sprintf "%s: %s" (get_input x.source.node) info in
              let cb   = new Checkbox.t () in
              new Item_list.Item.t ~ripple:true ~graphic:cb ~text ~value:() (),
              React.S.map (function true  -> Some x | false -> None) cb#s_state) sms
        in
        let lst = new Item_list.t ~items:(List.map (fun x -> `Item (fst x)) items) () in
        lst#set_id id;
        (try Dom.removeChild div (Dom_html.getElementById id) with _ -> ());
        Dom.appendChild div lst#root;
        let s = React.S.merge (fun acc x -> match x with
                                            | Some x -> x :: acc
                                            | None   -> acc) [] @@ List.map snd items
        in
        React.S.map (fun x -> s_push x) s |> ignore)
      (React.S.hold init.streams events.streams)
  in
  Widget.create div,s

let card control
      ~(init   : init)
      ~(events : events) =
  (* let title   = new Card.Title.t ~title:"Настройки" () in
   * let primary = new Card.Primary.t ~widgets:[title] () in *)
  let sms_lst,selected = streams ~init ~events in
  let apply   = new Button.t ~label:"Применить" () in
  let actions = new Card.Actions.t ~widgets:[apply] () in
  (* title#add_class "color--primary-on-primary";
   * primary#add_class "background--primary"; *)
  let card = new Card.t ~widgets:[ (new Card.Media.t ~widgets:[sms_lst] ())#widget
                                 ; actions#widget
               ] ()
  in
  React.E.map (fun _ -> Requests.Transmitter.HTTP.set_streams (React.S.value selected)
                          control) apply#e_click |> ignore;
  card

let layout control
      ~(init   : init)
      ~(events : events) =
  let card = card control ~init ~events in
  card

class settings control () = object(self)

  val mutable in_dom               = false
  val mutable state : state option = None
  val mutable observer             = None

  inherit Widget.t (Dom_html.createDiv Dom_html.document) ()

  method private observe =
    MutationObserver.observe
      ~node:Dom_html.document
      ~f:(fun _ _ ->
        let in_dom_new = (Js.Unsafe.coerce Dom_html.document)##contains self#root in
        if in_dom && (not in_dom_new)
        then Option.iter (fun (x:state) -> x.streams##close; x.config##close; state <- None) state
        else if (not in_dom) && in_dom_new
        then (let open Lwt_result.Infix in
              Requests.Transmitter.HTTP.get_mode control
              >>= (fun config ->
                Requests.Transmitter.HTTP.get_in_streams control
                >>= (fun streams ->
                     let config_ws,config_sock   = Requests.Transmitter.WS.get_mode control in
                     let streams_ws,streams_sock = Requests.Transmitter.WS.get_in_streams control in
                     Dom.appendChild self#root (layout control
                                                  ~init:{ streams; config }
                                                  ~events:{ streams = streams_ws
                                                          ; config = config_ws })#root;
                     state <- Some { streams = streams_sock
                                   ; config  = config_sock };
                     Lwt_result.return ()))
              |> ignore;
              in_dom <- in_dom_new))
      ~child_list:true
      ~subtree:true
      ()
    |> (fun o -> observer <- Some o)

  initializer
    self#observe

end
