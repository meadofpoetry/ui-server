open Components
open Lwt_result.Infix

let get_board_state control =
  Requests.Device.HTTP.get_state control
  >|= (fun state ->
    let e, sock = Requests.Device.WS.get_state control in
    React.S.hold state e, sock)
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let get_stream id control =
  Requests.Streams.HTTP.get_streams ~ids:[id] control
  >|= (fun streams ->
    let e, sock = Requests.Streams.WS.get_streams ~ids:[id] control in
    React.S.hold streams e, sock)
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let get_state id control =
  let open Widget_common in
  get_board_state control
  >>= fun (state, state_sock) -> get_stream id control
  >>= (fun (stream, stream_sock) ->
    React.S.l2 (fun state stream ->
        match state, stream with
        | `No_response, _ -> No_response
        | `Init, _ -> No_response
        | _, [] -> No_sync
        | _ -> Fine) state stream
    |> fun s -> Lwt_result.return (s, (fun () ->
                      React.S.stop ~strong:true s;
                      state_sock##close;
                      stream_sock##close)))

let wrap title thread =
  let _class = "qos-niit-widget-wrapper" in
  let title_class = Markup.CSS.add_element _class "title" in
  let icon = Icon.SVG.(create_simple Path.settings) in
  let button = new Icon_button.t ~disabled:true ~icon () in
  thread
  >|= (fun w ->
    button#set_disabled false;
    let settings = w#settings_widget in
    let dialog =
      let open Dialog in
      new t
        ~title:"Настройки отображения"
        ~content:(`Widgets [settings])
        ~actions:[ new Action.t ~typ:`Cancel ~label:"Отмена" ()
                 ; new Action.t ~typ:`Accept ~label:"Применить" () ]
        () in
    Dom.appendChild Dom_html.document##.body dialog#root;
    button#listen_click_lwt (fun _ _ ->
        Lwt.(settings#reset ();
             dialog#show_await ()
             >|= function `Accept -> settings#apply ()
                        | `Cancel -> ()))
    |> Lwt.ignore_result;
    let title = new Typography.Text.t ~text:title () in
    let title_box =
      new Hbox.t
        ~halign:`Space_between
        ~valign:`Center
        ~widgets:[title#widget; button#widget]
        () in
    let card = new Card.t ~widgets:[w] () in
    let box = new Vbox.t ~widgets:[title_box#widget; card#widget] () in
    title_box#add_class title_class;
    box#add_class _class;
    w, box)
