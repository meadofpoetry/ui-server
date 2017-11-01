open Lwt_react
open Board_ip_dektec_js.Requests
open Pipeline_js.Requests
open Hardware_js.Requests
open Components

let return = Lwt.return
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let (%) = CCFun.(%)

let button_type = Js.string "button"

let server =
  let protocol = (Js.to_string Dom_html.window##.location##.protocol) in
  protocol ^ "//" ^ (Js.to_string Dom_html.window##.location##.hostname) ^ ":8088/janus"

let janus_pipe debug =
  let open Janus_static in
  let e_msg, push_msg   = Lwt_react.E.create () in
  let e_jsep, push_jsep = Lwt_react.E.create () in
  let e_rs, push_rs     = Lwt_react.E.create () in

  init debug
  >>= (fun () -> let res = create ~server:(`One server) () in
                 (* FIXME do something useful in case of error*)
                 res.error >>= (fun s -> Printf.printf "Error in session handle %s\n" s |> return) |> ignore;
                 (* FIXME do something useful in case of destroy*)
                 res.destroy >>= (fun () -> Printf.printf "Session handle destroyed\n" |> return) |> ignore;
                 res.success)
  >>= (fun session -> Session.attach
                        ~session:session
                        ~plugin_type:Plugin.Streaming
                        ~on_remote_stream:push_rs
                        ~on_message:push_msg
                        ~on_jsep:push_jsep
                        ())
  >>= (fun plugin ->
    let _ = Lwt_react.E.map (fun x-> Printf.printf "Got a message: %s\n"
                                                   (Js.to_string @@ Json.output x)) e_msg in
    let _ = Lwt_react.E.map (fun stream -> Janus.attachMediaStream "remotevideo" stream) e_rs in
    let _ = Lwt_react.E.map (function
                             | Session.Offer x ->
                                Plugin.create_answer plugin Janus_streaming.default_media_props None x
                                >>= (function
                                     | Ok jsep -> return @@ (Janus_streaming.send ~jsep:jsep plugin Start |> ignore)
                                     | Error e -> return @@ Printf.printf "Error creating answer: %s\n" e) |> ignore
                             | Answer x        -> Plugin.handle_remote_jsep plugin x |> ignore
                             | Unknown _       -> Printf.printf "Unknown jsep received\n" |> ignore) e_jsep in
    return plugin)
  >>= (fun plugin -> Janus_streaming.send plugin (Watch { id = 1; secret = None }) |> ignore ; return ())

let onload _ =

  (*let streams, push_streams = S.create Js.null in*)
  
  let () = (Lwt.catch
              (fun () -> (janus_pipe (`All false)))
              (function
               | e -> return @@ Printf.printf "Exception in janus pipe: %s\n" (Printexc.to_string e)))
           |> ignore in

  let doc = Dom_html.document in

  let label    = Dom_html.createH2 doc in
  let button_set = Dom_html.createButton ~_type:(Js.string "button") doc in
  let button_reset = Dom_html.createButton ~_type:(Js.string "button") doc in
  button_set##.value := (Js.string "set");
  button_reset##.value := (Js.string "reset");
  let ev_label = Dom_html.createH2 doc in

  (* test *)

  Lwt.ignore_result @@ Lwt_js_events.clicks button_set (fun _ _ ->
                                              let data = Board_ip_dektec_js.Requests.post_delay 5 101 in
                                              data >>= function
                                              | Error e -> Lwt.return @@ (label##.textContent := Js.some @@ Js.string e)
                                              | Ok devi -> Lwt.return @@ (label##.textContent := Js.some @@ Js.string
                                                                                                 @@ Yojson.Safe.to_string
                                                                                                 @@ Board_ip_dektec_js.Board_types.delay_to_yojson devi));

  Lwt.ignore_result @@ Lwt_js_events.clicks button_reset (fun _ _ -> Lwt.return @@ (label##.textContent := Js.some @@ Js.string ""));

  let _ = React.E.map (fun x -> ev_label##.textContent := Js.some @@ Js.string (Yojson.Safe.to_string @@ Board_ip_dektec_js.Board_types.board_status_to_yojson x)) (Board_ip_dektec_js.Requests.get_status_socket 5) in

  let dialog   = Dialog.create ~description_id:"did"
                               ~label_id:"lid"
                               ~content:[ Dialog.Header.create ~id:"lid" ~label:"This is dialog" ()
                                        ; Dialog.Body.create ~id:"did" ~children:[] ()
                                        ; Dialog.Footer.create
                                            ~children:[ Dialog.Footer.create_button ~_type:`Decline
                                                                                    ~label:"Cancel"
                                                                                    ()
                                                      ; Dialog.Footer.create_button ~_type:`Accept
                                                                                    ~label:"Accept"
                                                                                    ()
                                                      ] ()
                                        ] () in
  Dom_html.addEventListener dialog
                            Dialog.events.accept
                            (Dom_html.handler (fun _ -> print_endline "accepted!"; Js._false))
                            Js._false
  |> ignore;
  let ac = Dom_html.getElementById "arbitrary-content" in
  Dom.appendChild ac dialog;
  let checkbox = Checkbox.create () in
  let switch = Switch.create ~input_id:"sw" () in
  let toggle = Icon_toggle.create ~on_content:"favorite"
                                  ~on_label:"Added to favorites"
                                  ~off_label:"Removed from favorites"
                                  ~off_content:"favorite_border"
                                  () in
  let tiles  = List.map (fun x -> let primary = Grid_list.Tile.create_primary
                                                  ~is_div:true
                                                  ~src:"https://cs5-3.4pda.to/5290239.png"
                                                  () in
                                  let secondary = Grid_list.Tile.create_secondary
                                                    ~title:("My tile " ^ (string_of_int x))
                                                    ~support_text:"Some text here"
                                                    () in
                                  Grid_list.Tile.create ~primary ~secondary ())
                        (CCList.range 0 15) in
  let grid   = Grid_list.create ~twoline:true
                                ~header_caption:true
                                ~icon_align:`Start
                                ~tiles
                                () in
  Dom_html.addEventListener switch
                            Dom_events.Typ.change
                            (Dom_html.handler (fun _ -> print_endline "changed on switch!"; Js._false))
                            Js._false
  |> ignore;
  Dom_html.addEventListener toggle
                            Icon_toggle.events.change
                            (Dom_html.handler (fun d -> print_endline ("Icon Toggle is " ^ (if (Js.to_bool d##.detail##.isOn)
                                                                                            then "on"
                                                                                            else "off"));
                                                        print_endline (Js.to_string d##._type);
                                                        Js._false))
                            Js._false
  |> ignore;
  let form_field = Form_field.create ~input:(of_dom switch)
                                     ~label:(Form_field.Label.create ~label:"this is a switch"
                                                                     ~for_id:"sw"
                                                                     ())
                                     () in
  Dom.appendChild ac (Button.create ~raised:true
                                    ~label:"show"
                                    ~ripple:true
                                    ~onclick:(fun _ -> dialog##.component_##show_ (); true)
                                    ());
  let cells = List.map (fun x -> Layout_grid.Cell.create ~content:[Tyxml_js.Html.pcdata (string_of_int x)]
                                                         ~span:{ columns = 1
                                                               ; device_type = None
                                                               }
                                                         ())
                       (CCList.range 0 15) in
  let layout_grid = Layout_grid.create ~content:[Layout_grid.create_inner ~cells ()]
                                       () in
  let linear_progress = Linear_progress.create () in
  let lp_ind_btn     = Button.create ~label:"indeterminate"
                                     ~onclick:(fun _ -> linear_progress##set_determinate_ Js._false; true)
                                     () in
  let lp_det_btn     = Button.create ~label:"determinate"
                                     ~onclick:(fun _ -> linear_progress##set_determinate_ Js._true; true)
                                     () in
  let progress0_btn  = Button.create ~label:"progress0"
                                     ~onclick:(fun _ -> linear_progress##set_progress_ (Js.number_of_float 0.); true)
                                     () in
  let progress20_btn = Button.create ~label:"progress20"
                                     ~onclick:(fun _ -> linear_progress##set_progress_ (Js.number_of_float 0.2); true)
                                     () in
  let progress60_btn = Button.create ~label:"progress60"
                                     ~onclick:(fun _ -> linear_progress##set_progress_ (Js.number_of_float 0.6); true)
                                     () in
  let buffer10_btn  = Button.create ~label:"buffer10"
                                    ~onclick:(fun _ -> linear_progress##set_buffer_ (Js.number_of_float 0.1); true)
                                    () in
  let buffer30_btn = Button.create ~label:"buffer30"
                                   ~onclick:(fun _ -> linear_progress##set_buffer_ (Js.number_of_float 0.3); true)
                                   () in
  let buffer70_btn = Button.create ~label:"buffer70"
                                   ~onclick:(fun _ -> linear_progress##set_buffer_ (Js.number_of_float 0.7); true)
                                   () in
  let open_btn = Button.create ~label:"open"
                               ~onclick:(fun _ -> linear_progress##open_ (); true)
                               () in
  let close_btn = Button.create ~label:"close"
                                ~onclick:(fun _ -> linear_progress##close_ (); true)
                                () in
  let btn_div_1      = Dom_html.createDiv Dom_html.document in
  let btn_div_2      = Dom_html.createDiv Dom_html.document in
  Dom.appendChild ac checkbox;
  Dom.appendChild ac form_field;
  Dom.appendChild ac toggle;
  Dom.appendChild ac grid;
  Dom.appendChild ac label;
  Dom.appendChild ac button_set;
  Dom.appendChild ac button_reset;
  Dom.appendChild ac ev_label;
  Dom.appendChild btn_div_1 lp_ind_btn;
  Dom.appendChild btn_div_1 lp_det_btn;
  Dom.appendChild btn_div_1 progress0_btn;
  Dom.appendChild btn_div_1 progress20_btn;
  Dom.appendChild btn_div_1 progress60_btn;
  Dom.appendChild btn_div_2 buffer10_btn;
  Dom.appendChild btn_div_2 buffer30_btn;
  Dom.appendChild btn_div_2 buffer70_btn;
  Dom.appendChild btn_div_2 open_btn;
  Dom.appendChild btn_div_2 close_btn;
  Dom.appendChild ac btn_div_1;
  Dom.appendChild ac btn_div_2;
  Dom.appendChild ac linear_progress;
  Dom.appendChild ac layout_grid;
  
  Js._false

let () = Dom_html.addEventListener Dom_html.document
                                   Dom_events.Typ.domContentLoaded
                                   (Dom_html.handler onload)
                                   Js._false
         |> ignore
