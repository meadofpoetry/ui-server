open Board_types
open Components

let t2mi_mode ~(init  : config)
              ~(event : config React.event) =
  let enabled = new Switch.t () in
  let pid     = new Textfield.t
                  ~input_id:"pid_field"
                  ~input_type:(Integer ((Some 0),(Some 8192)))
                  ~label:"T2-MI PID" () in
  let en_form = new Form_field.t ~input:enabled ~label:"Включить анализ" ~align_end:true () in
  pid#set_required true;
  (match init.mode.t2mi with
   | Some mode -> enabled#set_checked mode.enabled;
                  pid#fill_in mode.pid;
   | None      -> enabled#set_checked false);
  let s = React.S.l2 (fun en pid -> match pid with
                                    | Some pid -> Ok { enabled = en
                                                     ; pid
                                                     ; stream_id = Common.Stream.Single }
                                    | None     -> Error "pid value not provided")
                     enabled#s_state pid#s_input in
  let _ = React.E.map (fun config ->
              match config.mode.t2mi with
              | Some t2mi -> enabled#set_checked t2mi.enabled;
                             pid#fill_in t2mi.pid
              | None      -> enabled#set_checked false;
                             pid#clear) event
  in
  new Box.t ~widgets:[ en_form#widget; pid#widget ] (), s

let card control
         ~(init  : config)
         ~(event : config React.event) =
  (* let title       = new Card.Title.t ~title:"Настройки" () in
   * let primary     = new Card.Primary.t ~widgets:[title] () in *)
  let items       = [ `Item (new Select.Item.t ~text:"ASI" ~value:ASI ())
                    ; `Item (new Select.Item.t ~text:"SPI" ~value:SPI ())
                    ]
  in
  let inp         = new Select.t ~label:"Вход" ~items () in
  let _           = inp#set_selected_value ~eq:equal_input init.mode.input in
  let t2mi,s_t2mi = t2mi_mode ~init ~event in
  let common_sect = new Card.Media.t ~widgets:[inp#widget] () in
  let t2mi_sect   = new Card.Media.t ~widgets:[t2mi#widget] () in
  let apply       = new Button.t ~label:"Применить" () in
  let actions     = new Card.Actions.t ~widgets:[ apply ] () in
  (* title#add_class "color--primary-on-primary";
   * primary#add_class "background--primary"; *)
  let card    = new Card.t ~widgets:[ common_sect#widget
                                    ; t2mi_sect#widget
                                    ; actions#widget
                                    ] ()
  in
  let s = React.S.l2 (fun inp t2mi ->
              match inp,t2mi with
              | Some input, Ok t2mi -> Ok { input; t2mi = Some t2mi }
              | _, Error e          -> Error (Printf.sprintf "t2mi settings error: %s" e)
              | _                   -> Error "input not provided") inp#s_selected_value s_t2mi
  in
  let _ = React.E.map (fun config -> inp#set_selected_value ~eq:equal_input config.mode.input) event in
  let _ = React.E.map (fun _ -> match React.S.value s with
                                | Ok mode -> Requests.post_mode control mode
                                | Error e -> Lwt_result.fail e) apply#e_click
  in
  card

let layout control
           ~(init  : config)
           ~(event : config React.event) =
  let card = card control ~init ~event in
  let cell = new Layout_grid.Cell.t ~widgets:[card] () in
  let grid = new Layout_grid.t ~cells:[cell] () in
  grid

let page control =
  let open Lwt_result.Infix in
  let div = Dom_html.createDiv Dom_html.document in
  let t   =
    Requests.get_config control
    >>= (fun init ->
      let event,sock = Requests.get_config_ws control in
      Dom.appendChild div (layout control ~init ~event)#root;
      Lwt_result.return sock)
  in
  div,(fun () -> t >>= (fun x -> x##close; Lwt_result.return ()) |> ignore)
