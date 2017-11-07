open Lwt_react
open Components
open Tyxml_js

let demo_section ?(style="") title content =
  Html.section ~a:[ Html.a_style ("margin: 24px; padding: 24px;\
                                   border: 1px solid rgba(0, 0, 0, .12);" ^ style) ]
               ( Html.h2 ~a:[ Html.a_class [Typography.title_class]] [Html.pcdata title]
                 :: content)
  |> Tyxml_js.To_dom.of_element

let subsection name elt = Html.div [ Html.h3 ~a:[Html.a_class [Typography.caption_class]] [Html.pcdata name]
                                   ; elt ]

let button_demo () =
  let style      = "margin:10px;" in
  let raised     = Button.create ~style ~label:"raised" ~raised:true () in
  let flat       = Button.create ~style ~label:"flat" () in
  let unelevated = Button.create ~style ~unelevated:true ~label:"unelevated" () in
  let stroked    = Button.create ~style ~stroked:true ~label:"stroked" () in
  let ripple     = Button.create ~style ~label:"ripple" ~ripple:true () in
  let dense      = Button.create ~style ~label:"dense" ~raised:true ~dense:true () in
  let compact    = Button.create ~style ~label:"compact" ~raised:true ~compact:true () in
  demo_section ~style:"display:flex; \
                       flex-direction:column;\
                       justify-content:flex-start;\
                       align-items:flex-start"
               "Button"
               (List.map of_dom [raised;flat;unelevated;stroked;ripple;dense;compact])

let fab_demo () =
  let style  = "margin:10px;" in
  let fab    = subsection "General" (of_dom @@ Fab.create ~style ~icon:"favorite" ()) in
  let mini   = subsection "Mini" (of_dom @@ Fab.create ~style ~mini:true ~icon:"favorite" ()) in
  let ripple = subsection "Ripple" (of_dom @@ Fab.create ~style ~ripple:true ~icon:"favorite" ()) in
  demo_section "FAB" [fab;mini;ripple]

let checkbox_demo () =
  let checkbox = Checkbox.create ~input_id:"demo-checkbox" () in
  let form_field = Form_field.create ~label:(Form_field.Label.create ~label:"checkbox label"
                                                                     ~for_id:"demo-checkbox"
                                                                     ())
                                     ~input:(of_dom checkbox)
                                     () in
  let raw = subsection "Checkbox" (Checkbox.create () |> of_dom) in
  let labelled = subsection "Checkbox with label" (of_dom form_field) in
  demo_section "Checkbox" [ raw; labelled ]

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
  demo_section "Switch" [ raw; labelled ]

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

let card_demo () =
  let card = Card.create ~sections:[ Card.Media.create ~style:"background-image: url(\"https://maxcdn.icons8.com/app/uploads/2016/03/material-1-1000x563.jpg\");\
                                                               background-size: cover;\
                                                               background-repeat: no-repeat;\
                                                               height: 12.313rem;"
                                                       ~children:[]
                                                       ()
                                   ; Card.Primary.create
                                       ~children:[ Card.Primary.create_title ~large:true ~title:"Demo card title" ()
                                                 ; Card.Primary.create_subtitle ~subtitle:"Subtitle" ()]
                                       ()
                                   ; Card.Supporting_text.create
                                       ~children:[Html.pcdata "Supporting text"]
                                       ()
                                   ; Card.Actions.create
                                       ~children:[ of_dom @@ Button.create ~label:"Action 1" ()
                                                 ; of_dom @@ Button.create ~label:"Action 2" ()]
                                       ()]
                         ~style:"width:320px;"
                         () in
  demo_section "Card" [ of_dom card ]

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

let layout_grid_demo () =
  let cells = List.map (fun x -> Layout_grid.Cell.create ~style:"box-sizing: border-box;\
                                                                 background-color: #666666;\
                                                                 height: 200px;\
                                                                 padding: 8px;\
                                                                 color: white;\
                                                                 font-size: 1.5em;"
                                                         ~content:[Html.pcdata ( "id=" ^ (string_of_int x)
                                                                                 ^ "\nspan="
                                                                                 ^ (string_of_int (if x = 3
                                                                                                   then 2
                                                                                                   else 1)))]
                                                         ~span:{ columns = if x = 3 then 2 else 1
                                                               ; device_type = None
                                                               }
                                                         ())
                       (CCList.range 0 15) in
  let layout_grid = Layout_grid.create ~content:[Layout_grid.create_inner ~cells ()]
                                       () in
  demo_section "Layout grid" [ of_dom layout_grid ]

let dialog_demo () =
  let dialog = Dialog.create ~description_id:"did"
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
  let button = Button.create ~raised:true
                             ~label:"show dialog"
                             ~ripple:true
                             ~onclick:(fun _ -> dialog##.component_##show_ (); true)
                             () in
  Dom_html.addEventListener dialog
                            Dialog.events.accept
                            (Dom_html.handler (fun _ -> print_endline "Dialog accepted!"; Js._false))
                            Js._false |> ignore;
  Dom_html.addEventListener dialog
                            Dialog.events.cancel
                            (Dom_html.handler (fun _ -> print_endline "Dialog cancelled!"; Js._false))
                            Js._false |> ignore;
  demo_section "Dialog" [ of_dom dialog; of_dom button ]

let list_demo () =
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
  let list = List_.create ~items:list_items
                          ~avatar:true
                          ~two_line:true
                          ~style:"max-width: 400px;"
                          () in
  demo_section "List" [ of_dom list ]

let menu_demo () =
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
                                  ~raised:true
                                  ~onclick:(fun _ -> menu##show_ (Some ({focus_index = 2} : Menu.show )); true)
                                  () in
  let menu_div = Html.div ~a:[Tyxml_js.Html.a_class [Menu.anchor_class]]
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
  demo_section "Menu" [ menu_div ]

let linear_progress_demo () =
  let linear_progress = Linear_progress.create ~indeterminate:true () in
  let lp_ind_btn = Button.create ~label:"set indeterminate"
                                 ~raised:true
                                 ~onclick:(fun _ -> linear_progress##set_determinate_ Js._false; true)
                                 () in
  let lp_det_btn = Button.create ~label:"set determinate"
                                 ~raised:true
                                 ~onclick:(fun _ -> linear_progress##set_determinate_ Js._true; true)
                                 () in
  let progress0_btn = Button.create ~label:"set progress 0"
                                    ~raised:true
                                    ~onclick:(fun _ -> linear_progress##set_progress_ (Js.number_of_float 0.); true)
                                    () in
  let progress20_btn = Button.create ~label:"set progress 20"
                                     ~raised:true
                                     ~onclick:(fun _ -> linear_progress##set_progress_ (Js.number_of_float 0.2); true)
                                     () in
  let progress60_btn = Button.create ~label:"set progress 60"
                                     ~raised:true
                                     ~onclick:(fun _ -> linear_progress##set_progress_ (Js.number_of_float 0.6); true)
                                     () in
  let buffer10_btn = Button.create ~label:"set buffer 10"
                                   ~raised:true
                                   ~onclick:(fun _ -> linear_progress##set_buffer_ (Js.number_of_float 0.1); true)
                                   () in
  let buffer30_btn = Button.create ~label:"set buffer 30"
                                   ~raised:true
                                   ~onclick:(fun _ -> linear_progress##set_buffer_ (Js.number_of_float 0.3); true)
                                   () in
  let buffer70_btn = Button.create ~label:"set buffer 70"
                                   ~raised:true
                                   ~onclick:(fun _ -> linear_progress##set_buffer_ (Js.number_of_float 0.7); true)
                                   () in
  let open_btn = Button.create ~label:"open"
                               ~raised:true
                               ~onclick:(fun _ -> linear_progress##open_ (); true)
                               () in
  let close_btn = Button.create ~label:"close"
                                ~raised:true
                                ~onclick:(fun _ -> linear_progress##close_ (); true)
                                () in
  let cells = List.map (fun x -> Layout_grid.Cell.create ~content:[of_dom x]
                                                         ~span:{ columns = 12
                                                               ; device_type = None
                                                               }
                                                         ())
                       [ lp_ind_btn
                       ; lp_det_btn
                       ; progress0_btn
                       ; progress20_btn
                       ; progress60_btn
                       ; buffer10_btn
                       ; buffer30_btn
                       ; buffer70_btn
                       ; open_btn
                       ; close_btn ] in
  let btn_grid = Layout_grid.create ~content:[Layout_grid.create_inner ~cells ()] () in
  demo_section "Linear progress" [ of_dom btn_grid; of_dom linear_progress ]

let tabs_demo () =
  let tab_items = List.map (fun x -> Tabs.Tab.create ~content:(`Text ("Tab " ^ (string_of_int x))) ())
                           (CCList.range 0 4) in
  let tabs = Tabs.create ~classes:[Tabs.Scroller.scroll_frame_tabs_class]
                         ~_type:`Text
                         ~style:"margin-bottom: 30px"
                         ~content:tab_items () in
  let scroller = Tabs.Scroller_.create ~tabs:(of_dom tabs) () in
  demo_section "Tabs" [ of_dom scroller ]

let add_demos parent demos =
  List.iter (fun x -> Dom.appendChild parent x) demos

let onload _ =
  let doc = Dom_html.document in
  let body = doc##.body in
  add_demos body [ button_demo ()
                 ; fab_demo ()
                 ; checkbox_demo ()
                 ; switch_demo ()
                 ; toggle_demo ()
                 ; card_demo ()
                 ; slider_demo ()
                 ; grid_list_demo ()
                 ; ripple_demo ()
                 ; layout_grid_demo ()
                 ; dialog_demo ()
                 ; list_demo ()
                 ; menu_demo ()
                 ; linear_progress_demo ()
                 ; tabs_demo ()
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
