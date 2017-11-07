open Lwt_react
open Components
open Tyxml_js

let demo_section title content =
  Html.section ~a:[ Html.a_style "margin: 24px; padding: 24px;\
                                  border: 1px solid rgba(0, 0, 0, .12);" ]
               ( Html.h2 ~a:[ Html.a_class [Typography.title_class]] [Html.pcdata title]
                 :: content)
  |> Tyxml_js.To_dom.of_element

let subsection name elt = Html.div [ Html.h3 ~a:[Html.a_class [Typography.caption_class]] [Html.pcdata name]
                                   ; elt ]

let checkbox_demo () =
  let checkbox = Checkbox.create () in
  demo_section "Checkbox" [ of_dom checkbox ]

let switch_demo () =
  let switch = Switch.create ~input_id:"demo-switch" () in
  let form_field = Form_field.create ~label:(Form_field.Label.create ~label:"switch label"
                                                                     ~for_id:"demo-switch"
                                                                     ())
                                     ~input:(of_dom switch)
                                     () in
  let raw = subsection "Switch" (Switch.create () |> of_dom) in
  let labelled = subsection "Switch with label" (of_dom form_field) in
  Dom_html.addEventListener (switch##querySelector (Js.string ("." ^ Switch.native_control_class))
                             |> Js.Opt.to_option
                             |> CCOpt.get_exn)
                            Dom_events.Typ.change
                            (Dom_html.handler (fun _ ->
                                 let s = "Switch is " ^ (if (Js.to_bool (switch##is_checked_ ()))
                                                         then "on"
                                                         else "off") in
                                 print_endline s;
                                 Js._false))
                            Js._false |> ignore;
  demo_section "Switch" [ raw
                        ; labelled
                        ]

let toggle_demo () =
  let toggle = Icon_toggle.create ~on_content:"favorite"
                                  ~on_label:"Added to favorites"
                                  ~off_label:"Removed from favorites"
                                  ~off_content:"favorite_border"
                                  () in
  Dom_html.addEventListener toggle
                            Icon_toggle.events.change
                            (Dom_html.handler (fun e ->
                                 print_endline ("Icon Toggle is " ^ (if (Js.to_bool e##.detail_##.isOn)
                                                                     then "on"
                                                                     else "off"));
                                 Js._false))
                            Js._false |> ignore;
  demo_section "Icon toggle" [ of_dom toggle ]

let slider_demo () =
  let listen elt name = 
    Dom_html.addEventListener elt
                              Slider.events.input
                              (Dom_html.handler (fun _ ->
                                   print_endline (Printf.sprintf "Input event on %s slider!" name);
                               Js._false))
                              Js._false |> ignore;
    Dom_html.addEventListener elt
                              Slider.events.change
                              (Dom_html.handler (fun e ->
                                   print_endline ((Printf.sprintf "Change event on %s slider! " name)
                                                  ^ (e##.detail_##.value_
                                                     |> Js.float_of_number
                                                     |> string_of_float));
                                   Js._false))
                              Js._false |> ignore in
  let continuous = Slider.create ~label:"Select Value"
                                 ~id:"continuous-slider"
                                 ~style:"max-width: 700px;"
                                 ~value:0
                                 () in
  let discrete  = Slider.create ~label:"Select Value"
                                ~id:"discrete-slider"
                                ~style:"max-width: 700px;"
                                ~value:0
                                ~discrete:true
                                () in
  let with_markers = Slider.create ~label:"Select Value"
                                   ~id:"markered-slider"
                                   ~style:"max-width: 700px;"
                                   ~value:0
                                   ~discrete:true
                                   ~markers:true
                                   () in
  let disabled = Slider.create ~style:"max-width: 700px;"
                               ~disabled:true
                               () in
  listen continuous "continuous";
  listen discrete "discrete";
  listen with_markers "markered";
  demo_section "Slider" [ subsection "Continuous slider" (of_dom continuous)
                        ; subsection "Discrete slider" (of_dom discrete)
                        ; subsection "Discrete slider with markers" (of_dom with_markers)
                        ; subsection "Disabled slider" (of_dom disabled) ]

let grid_list_demo () =
  let tiles  = List.map (fun x -> let primary = Grid_list.Tile.create_primary
                                                  ~src:"https://cs5-3.4pda.to/5290239.png"
                                                  () in
                                  let secondary = Grid_list.Tile.create_secondary
                                                    ~title:("My tile " ^ (string_of_int x))
                                                    ~support_text:"Some text here"
                                                    () in
                                  Grid_list.Tile.create ~primary ~secondary ())
                        (CCList.range 0 4) in
  let grid   = Grid_list.create ~twoline:true
                                ~header_caption:true
                                ~tiles
                                () in
  demo_section "Grid list" [ of_dom grid ]

let ripple_demo () =
  let bounded = (Html.div ~a:[ Html.a_class [ Ripple.base_class
                                                          ; Elevation.get_elevation_class 4 ]
                                           ; Html.a_style "width: 200px; height: 150px" ] []
                               |> (fun x -> Ripple.create x ())
                               |> of_dom) in
  let ripple_div = subsection "Bounded ripple. Click me!" bounded in
  let unbounded_div = subsection "Unbounded ripple. Click me!"
                                 (Html.div ~a:[ Html.a_class [ Ripple.base_class
                                                             ; "material-icons" ]
                                              ; Ripple.unbounded_attr
                                              ; Html.a_style "user-select:none;"
                                              ; Html.a_id "unbounded-ripple" ] [Html.pcdata "favorite"]
                                  |> (fun x -> Ripple.create x ())
                                  |> of_dom) in
  demo_section "Ripple" [ ripple_div
                        ; unbounded_div ]

let add_demos parent demos =
  List.iter (fun x -> Dom.appendChild parent x) demos

let onload _ =
  let doc = Dom_html.document in
  let body = doc##.body in
  add_demos body [ checkbox_demo ()
                 ; switch_demo ()
                 ; toggle_demo ()
                 ; slider_demo ()
                 ; grid_list_demo ()
                 ; ripple_demo ()
                 ];
  List.iter (fun x -> (body##querySelector (Js.string x)
                       |> Js.Opt.to_option
                       |> CCOpt.get_exn
                       |> Js.Unsafe.coerce)##layout_ () |> ignore)
            [ "#continuous-slider"; "#discrete-slider"; "#markered-slider" ];
  (body##querySelector (Js.string "#unbounded-ripple")
   |> Js.Opt.to_option
   |> CCOpt.get_exn
   |> Js.Unsafe.coerce)##layout_ () |> ignore;
  Js._false

let () = Dom_html.addEventListener Dom_html.document
                                   Dom_events.Typ.domContentLoaded
                                   (Dom_html.handler onload)
                                   Js._false
         |> ignore
