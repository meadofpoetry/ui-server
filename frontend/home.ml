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
  let switch = Switch.create ~input_id:"sw" () in
  Dom_html.addEventListener switch
                            Dom_events.Typ.change
                            (Dom_html.handler (fun _ -> print_endline "changed on switch!"; Js._false))
                            Js._false
  |> ignore;
  let form_field = Form_field.create ~input:(of_dom switch)
                                     ~label:(Form_field.Label.create ~label:"this is a switch"
                                                                     ~for_id:"sw"
                                                                     ())
                                     () in
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
  let btn_div_1 = Dom_html.createDiv Dom_html.document in
  let btn_div_2 = Dom_html.createDiv Dom_html.document in
  let list_items = List.map (fun x -> let checkbox = (Checkbox.create
                                                        ~attrs:[Tyxml_js.Html.a_onclick (fun e ->
                                                               Dom_html.stopPropagation e; true)]
                                                        ~style:"width:24px; height:18px"
                                                        ~classes:[List_.Item.end_detail_class]
                                                        ()) in
                                      if x != 2
                                      then List_.Item.create
                                             ~attrs:[Tyxml_js.Html.a_onclick (fun _ ->
                                                         let value = checkbox##is_checked_ ()
                                                                     |> Js.to_bool
                                                                     |> not
                                                                     |> Js.bool in
                                                         checkbox##set_checked_ value; true)]
                                             ~text:("List item " ^ (string_of_int x))
                                             ~secondary_text:"some subtext here"
                                             ~start_detail:(let open Tyxml_js.Html in
                                                            span
                                                              ~a:[ a_class [List_.Item.start_detail_class]
                                                                 ; a_style "background-color: lightgrey;\
                                                                            display: inline-flex;\
                                                                            align-items: center;\
                                                                            justify-content: center;"]
                                                              [ i ~a:[ a_class ["material-icons"]
                                                                     ; a_style "color: white;"]
                                                                  [pcdata "folder"]])
                                             ~end_detail:(checkbox |> of_dom)
                                             ~ripple:true
                                             ()
                                      else List_.Item.create_divider ())
                            (CCList.range 0 5) in
  let list      = List_.create ~items:list_items
                               ~avatar:true
                               ~two_line:true
                               ~style:"max-width: 400px;"
                               () in
  let menu_items = List.map (fun x -> if x != 2
                                      then Menu.Item.create
                                             ~text:("Menu item " ^ (string_of_int x))
                                             ~disabled:(x = 4)
                                             ()
                                      else Menu.Item.create_divider ())
                            (CCList.range 0 5) in
  let menu = Menu.create ~items:menu_items
                         () in
  let menu_anchor = Button.create ~label:"Open menu"
                                  ~onclick:(fun _ -> menu##show_ (Some ({focus_index = 2} : Menu.show )); true)
                                  () in
  let menu_div = Tyxml_js.Html.div ~a:[Tyxml_js.Html.a_class [Menu.anchor_class]]
                                   [of_dom menu_anchor; of_dom menu] in
  Dom_html.addEventListener menu
                            Menu.events.selected
                            (Dom_html.handler (fun d ->
                                 print_endline ("Selected menu item is " ^ (d##.detail_##.index_
                                                                            |> Js.float_of_number
                                                                            |> int_of_float
                                                                            |> string_of_int));
                                 Js._false))
                            Js._false
  |> ignore;
  Dom_html.addEventListener menu
                            Menu.events.cancel
                            (Dom_html.handler (fun _ -> print_endline "Menu cancelled"; Js._false))
                            Js._false
  |> ignore;
  let ripple_div = Tyxml_js.Html.(div ~a:[ a_class [ Ripple.base_class
                                                   ; Elevation.get_elevation_class 4
                                                   ]
                                         ; a_style "width: 300px; height: 300px"
                                         ]
                                      [pcdata "Here is a ripple"])
                   |> fun x -> Ripple.create x () in
  let tab_items = List.map (fun x -> Tabs.Tab.create ~content:(`Text (string_of_int x)) ())
                           (CCList.range 0 4) in
  let tabs = Tabs.create ~classes:[Tabs.Scroller.scroll_frame_tabs_class]
                         ~_type:`Text
                         ~style:"margin-bottom: 30px"
                         ~content:tab_items () in
  let scroller = Tabs.Scroller_.create ~tabs:(of_dom tabs) () in
  let sections = List.map (fun x -> Toolbar.Row.Section.create ~content:[ Tyxml_js.Html.pcdata (string_of_int x)]
                                                               ())
                          (CCList.range 0 1) in
  let rows = [ Toolbar.Row.create ~content:[] ()
             ; Toolbar.Row.create ~content:sections () ] in
  let toolbar = Toolbar.create ~content:rows
                               ~fixed:true
                               ~waterfall:true
                               () in
  toolbar##set_fixed_adjust_elt_ (Dom_html.document##.body##querySelector (Js.string ".mdc-toolbar-fixed-adjust")
                                  |> Js.Opt.to_option
                                  |> CCOpt.get_exn);
  Dom_html.addEventListener toolbar
                            Toolbar.events.change
                            (Dom_html.handler (fun e ->
                                 print_endline ("Change on toolbar! " ^ (e##.detail_##.flexibleExpansionRatio
                                                                         |> Js.float_of_number
                                                                         |> string_of_float));
                                 Js._false))
                            Js._false
  |> ignore;
  Dom.appendChild ac label;
  Dom.appendChild ac button_set;
  Dom.appendChild ac button_reset;
  Dom.appendChild ac ev_label;
  Dom.appendChild ac (Button.create ~raised:true
                                    ~label:"show"
                                    ~ripple:true
                                    ~onclick:(fun _ -> dialog##.component_##show_ (); true)
                                    ());
  Dom.appendChild ac form_field;
  Dom.appendChild ac (Tyxml_js.To_dom.of_element menu_div);
  Dom.appendChild ac ripple_div;
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
  Dom.appendChild ac list;
  Dom.appendChild ac scroller;
  Dom.appendChild ac (Tyxml_js.Html.div ~a:[Tyxml_js.Html.a_style "height:200px"] [] |> Tyxml_js.To_dom.of_element);

  Js._false

let () = Dom_html.addEventListener Dom_html.document
                                   Dom_events.Typ.domContentLoaded
                                   (Dom_html.handler onload)
                                   Js._false
         |> ignore
