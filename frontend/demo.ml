open Lwt_react
open Components
open Tyxml_js

let demo_section ?(style="") ?(classes=[]) title content =
  Html.section ~a:[ Html.a_style ("margin: 24px; padding: 24px;\
                                   border: 1px solid rgba(0, 0, 0, .12);" ^ style)
                  ; Html.a_class classes ]
               ( Html.h2 ~a:[ Html.a_class [Typography.headline_class]] [Html.pcdata title]
                 :: content)
  |> Tyxml_js.To_dom.of_element

let subsection name elt = Html.div [ Html.h3 ~a:[Html.a_class [Typography.subheading2_class]]
                                             [Html.pcdata name]
                                   ; elt ]

let button_demo () =
  let style      = "10px" in
  let raised     = let b = new Button.t ~label:"raised" () in
                   b#raised; b in
  let flat       = new Button.t ~raised:false ~label:"flat" () in
  let unelevated = let b = new Button.t ~label:"unelevated" () in
                   b#unelevated; b in
  let stroked    = let b = new Button.t ~label:"stroked" ~raised:false () in
                   b#stroked; b in
  let ripple     = new Button.t ~label:"ripple" ~ripple:true () in
  let dense      = let b = new Button.t ~label:"dense" () in
                   b#dense; b in
  let compact    = let b = new Button.t ~label:"compact" () in
                   b#compact; b in
  let icon       = new Button.t ~label:"icon" ~icon:"favorite" () in
  demo_section ~style:"display:flex; \
                       flex-direction:column;\
                       justify-content:flex-start;\
                       align-items:flex-start"
               "Button"
  @@ List.map (fun x -> x#style##.margin := Js.string style;
                        of_dom x#root)
              [raised;flat;unelevated;stroked;ripple;dense;compact;icon]

let fab_demo () =
  let fab    = subsection "General" @@ of_dom @@ (new Fab.t ~icon:"favorite" ())#root in
  let mini   = subsection "Mini"    @@ of_dom @@ (let b = new Fab.t ~icon:"favorite" ()
                                                  in b#mini; b#root) in
  let ripple = subsection "Ripple"  @@ of_dom @@ (new Fab.t ~ripple:true ~icon:"favorite" ())#root in
    demo_section "FAB" [fab;mini;ripple]

let radio_demo () =
  let radio1 = new Radio.t ~name:"radio" () in
  let radio2 = new Radio.t ~name:"radio" () in
  let radio3 = new Radio.t ~name:"radio" () in
  radio2#disable;
  demo_section "Radio button" [ of_dom radio1#root
                              ; of_dom radio2#root
                              ; of_dom radio3#root ]

let checkbox_demo () =
  let checkbox = new Checkbox.t ~input_id:"checkbox-demo" () in
  let form_field = new Form_field.t ~label:"checkbox label" ~input:checkbox () in
  (* let raw      = subsection "Checkbox (css only)" @@ Checkbox.create () in *)
  (* print_endline @@ string_of_bool checkbox#checked; *)
  let labelled = subsection "Checkbox with label" (of_dom form_field#root) in
  demo_section "Checkbox" [ labelled ]

let switch_demo () =
  let switch = new Switch.t ~input_id:"demo-switch" () in
  let form_field = new Form_field.t ~label:"switch label" ~input:switch () in
  let raw = subsection "Switch" @@ of_dom (new Switch.t ())#root in
  let labelled = subsection "Switch with label" (of_dom form_field#root) in
  Dom_html.addEventListener switch#input
                            Dom_events.Typ.change
                            (Dom_html.handler (fun _ ->
                                 let s = "Switch is " ^ (if switch#checked then "on" else "off") in
                                 print_endline s;
                                 Js._false))
                            Js._false |> ignore;
  demo_section "Switch" [ raw; labelled ]

 let toggle_demo () =
   let toggle = new Icon_toggle.t
                    ~on_data:{ icon = "favorite"; label = None; css_class = None }
                    ~off_data:{ icon = "favorite_border"; label = None; css_class = None }
                    () in
   Dom_html.addEventListener toggle#root
                             Icon_toggle.events.change
                             (Dom_html.handler (fun e ->
                                  print_endline ("Icon Toggle is " ^ (if (Js.to_bool e##.detail_##.isOn)
                                                                      then "on"
                                                                      else "off"));
                                  Js._false))
                             Js._false |> ignore;
  demo_section "Icon toggle" [ of_dom toggle#root ]

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
                                       ~children:[ (let b = new Button.t ~label:"action 1" () in
                                                    b#add_class Card.Actions.action_class;
                                                    b#compact;
                                                    of_dom b#root)
                                                 ; (let b = new Button.t ~label:"action 2" () in
                                                    b#add_class Card.Actions.action_class;
                                                    b#compact;
                                                    of_dom b#root)
                                                 ]
                                       ()]
                         ~style:"width:320px;"
                         () in
  demo_section "Card" [ card ]

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
                                 ()
                   |> Slider.attach in
  let discrete  = Slider.create ~label:"Select Value"
                                ~id:"discrete-slider"
                                ~style:"max-width: 700px;"
                                ~value:0
                                ~discrete:true
                                ()
                  |> Slider.attach in
  let with_markers = Slider.create ~label:"Select Value"
                                   ~id:"markered-slider"
                                   ~style:"max-width: 700px;"
                                   ~value:0
                                   ~discrete:true
                                   ~markers:true
                                   ()
                   |> Slider.attach in
  let disabled = Slider.create ~style:"max-width: 700px;"
                               ~disabled:true
                               ()
                 |> Slider.attach in
  listen continuous##.root__ "continuous";
  listen discrete##.root__ "discrete";
  listen with_markers##.root__ "markered";
  Dom_html.setTimeout (fun () -> continuous##layout_ ();
                                 discrete##layout_ ();
                                 with_markers##layout_ ()) 100. |> ignore;
  demo_section "Slider" [ subsection "Continuous slider" @@ of_dom continuous##.root__
                        ; subsection "Discrete slider" @@ of_dom discrete##.root__
                        ; subsection "Discrete slider with markers" @@ of_dom with_markers##.root__
                        ; subsection "Disabled slider" @@ of_dom disabled##.root__ ]

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
  demo_section "Grid list" [ grid ]

let ripple_demo () =
  let bounded = (Html.div ~a:[ Html.a_class [ Ripple.base_class
                                                          ; Elevation.get_elevation_class 4 ]
                                           ; Html.a_style "width: 200px; height: 150px" ] []
                 |> Ripple.attach) in
  let ripple_div = subsection "Bounded ripple. Click me!" @@ of_dom bounded##.root__ in
  let unbounded = (Html.div ~a:[ Html.a_class [ Ripple.base_class
                                                             ; "material-icons" ]
                                              ; Ripple.unbounded_attr
                                              ; Html.a_style "user-select:none;"
                                              ; Html.a_id "unbounded-ripple" ] [Html.pcdata "favorite"]
                                  |> Ripple.attach) in
  let unbounded_div = subsection "Unbounded ripple. Click me!" @@ of_dom unbounded##.root__ in
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
  demo_section "Layout grid" [ layout_grid ]

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
                                      ] ()
               |> Dialog.attach in
  let button = new Button.t ~label:"show dialog" ~ripple:true () in
  Dom_html.addEventListener dialog##.root__
                            Dialog.events.accept
                            (Dom_html.handler (fun _ -> print_endline "Dialog accepted!"; Js._false))
                            Js._false |> ignore;
  Dom_html.addEventListener dialog##.root__
                            Dialog.events.cancel
                            (Dom_html.handler (fun _ -> print_endline "Dialog cancelled!"; Js._false))
                            Js._false |> ignore;
  demo_section "Dialog" [ of_dom dialog##.root__; of_dom button#root ]

let list_demo () =
  let list_items = List.map (fun x -> (* let checkbox = (Checkbox.create
                                       *                   ~attrs:[Tyxml_js.Html.a_onclick (fun e ->
                                       *                               Dom_html.stopPropagation e; true)]
                                       *                   ~style:"width:24px; height:18px"
                                       *                   ~classes:[List_.Item.end_detail_class]
                                       *                   ()
                                       *                 |> Checkbox.attach) in
                                       * Checkbox.listen_change checkbox
                                       *                        (fun _ _ -> print_endline "changed!"; true)
                                       * |> ignore; *)
                                      if x != 2
                                      then List_.Item.create
                                             (* ~attrs:[Tyxml_js.Html.a_onclick (fun _ ->
                                              *             let value = checkbox##.checked_
                                              *                         |> Js.to_bool
                                              *                         |> not
                                              *                         |> Js.bool in
                                              *             checkbox##.checked_ := value; true)] *)
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
                                             (* ~end_detail:(checkbox##.root__ |> of_dom) *)
                                             ~auto_init:true
                                             ()
                                      else List_.Item.create_divider ())
                            (CCList.range 0 5) in
  let list = List_.create ~items:list_items
                          ~avatar:true
                          ~two_line:true
                          ~style:"max-width: 400px;"
                          () in
  let tree = (let items = List.map (fun x ->
                              let inner_items = List.map
                                                  (fun x -> List_.Item.create
                                                              ~text:("Inner " ^ (string_of_int x))
                                                              (* ~end_detail:(Checkbox.create 
                                                               *                ~style:"width:24px; height:18px"
                                                               *                ~classes:[List_.Item.end_detail_class]
                                                               *                ()) *)
                                                              ~auto_init:true
                                                              ())
                                                  (CCList.range 0 5) in
                              let inner = List_.create ~items:inner_items
                                                       ~style:"padding-right: 0px"
                                                       ~classes:["hide"]
                                                       ()
                                          |> Tyxml_js.To_dom.of_element in
                              Html.div [ List_.Item.create
                                           ~attrs:[Tyxml_js.Html.a_onclick (fun _ ->
                                                       inner##.classList##toggle (Js.string "hide")
                                                       |> ignore;
                                                       true)]
                                           ~text:("List item " ^ (string_of_int x))
                                           (* ~end_detail:(Checkbox.create 
                                            *              ~style:"width:24px; height:18px"
                                            *              ~classes:[List_.Item.end_detail_class]
                                            *              ()) *)
                                           ~auto_init:true
                                           ()
                                       ; Tyxml_js.Of_dom.of_element inner ])
                                   (CCList.range 0 5) in
              List_.create ~items
                           ~style:"max-width: 400px;"
                           ()) in
  demo_section "List" [ list; tree ]

let menu_demo () =
  let menu_items = List.map (fun x -> if x != 2
                                      then Menu.Item.create
                                             ~text:("Menu item " ^ (string_of_int x))
                                             ~disabled:(x = 4)
                                             ()
                                      else Menu.Item.create_divider ())
                            (CCList.range 0 5) in
  let menu = Menu.create ~items:menu_items () |> Menu.attach in
  let menu_anchor = new Button.t ~label:"Open menu" () in
  let menu_div = Html.div ~a:[Tyxml_js.Html.a_class [Menu.anchor_class]]
                          [ of_dom menu_anchor#root; of_dom menu##.root__ ] in
  Dom_html.addEventListener menu##.root__
                            Menu.events.selected
                            (Dom_html.handler (fun d ->
                                 print_endline ("Selected menu item is " ^ (d##.detail_##.index_
                                                                            |> Js.float_of_number
                                                                            |> int_of_float
                                                                            |> string_of_int));
                                 Js._false))
                            Js._false
  |> ignore;
  Dom_html.addEventListener menu##.root__
                            Menu.events.cancel
                            (Dom_html.handler (fun _ -> print_endline "Menu cancelled"; Js._false))
                            Js._false
  |> ignore;
  demo_section "Menu" [ menu_div ]

let linear_progress_demo () =
  let linear_progress = Linear_progress.create ~indeterminate:true ()
                        |> Linear_progress.attach in
  (* let lp_ind_btn = Button.create ~label:"set indeterminate"
   *                                ~raised:true
   *                                ~onclick:(fun _ -> linear_progress##.determinate_ := Js._false; true)
   *                                () in
   * let lp_det_btn = Button.create ~label:"set determinate"
   *                                ~raised:true
   *                                ~onclick:(fun _ -> linear_progress##.determinate_ := Js._true; true)
   *                                () in
   * let progress0_btn = Button.create ~label:"set progress 0"
   *                                   ~raised:true
   *                                   ~onclick:(fun _ -> linear_progress##.progress_ := (Js.number_of_float 0.);
   *                                                      true)
   *                                   () in
   * let progress20_btn = Button.create ~label:"set progress 20"
   *                                    ~raised:true
   *                                    ~onclick:(fun _ -> linear_progress##.progress_ := (Js.number_of_float 0.2);
   *                                                       true)
   *                                    () in
   * let progress60_btn = Button.create ~label:"set progress 60"
   *                                    ~raised:true
   *                                    ~onclick:(fun _ -> linear_progress##.progress_ := (Js.number_of_float 0.6);
   *                                                       true)
   *                                    () in
   * let buffer10_btn = Button.create ~label:"set buffer 10"
   *                                  ~raised:true
   *                                  ~onclick:(fun _ -> linear_progress##.buffer_ := (Js.number_of_float 0.1);
   *                                                     true)
   *                                  () in
   * let buffer30_btn = Button.create ~label:"set buffer 30"
   *                                  ~raised:true
   *                                  ~onclick:(fun _ -> linear_progress##.buffer_ := (Js.number_of_float 0.3);
   *                                                     true)
   *                                  () in
   * let buffer70_btn = Button.create ~label:"set buffer 70"
   *                                  ~raised:true
   *                                  ~onclick:(fun _ -> linear_progress##.buffer_ := (Js.number_of_float 0.7); true)
   *                                  () in
   * let open_btn = Button.create ~label:"open"
   *                              ~raised:true
   *                              ~onclick:(fun _ -> linear_progress##open_ (); true)
   *                              () in
   * let close_btn = Button.create ~label:"close"
   *                               ~raised:true
   *                               ~onclick:(fun _ -> linear_progress##close_ (); true)
   *                               () in
   * let cells = List.map (fun x -> Layout_grid.Cell.create ~content:[x]
   *                                                        ~span:{ columns = 12
   *                                                              ; device_type = None
   *                                                              }
   *                                                        ())
   *                      [ lp_ind_btn
   *                      ; lp_det_btn
   *                      ; progress0_btn
   *                      ; progress20_btn
   *                      ; progress60_btn
   *                      ; buffer10_btn
   *                      ; buffer30_btn
   *                      ; buffer70_btn
   *                      ; open_btn
   *                      ; close_btn ] in
   * let btn_grid = Layout_grid.create ~content:[Layout_grid.create_inner ~cells ()] () in *)
  demo_section "Linear progress" [ (* btn_grid; *) of_dom linear_progress##.root__ ]

let tabs_demo () =
  let icon_section = let items = List.map (fun x ->
                                     Tabs.Tab.create ~content:(`Icon (match x with
                                                                      | 0 -> "pets",None
                                                                      | 1 -> "favorite",None
                                                                      | 2 -> "grade",None
                                                                      | _ -> "room",None))
                                                     ())
                                          (CCList.range 0 3) in
                     let tabs = Tabs.Tab_bar.create ~_type:`Icon
                                                    ~content:items ()
                                |> Tabs.Tab_bar.attach in
                     subsection "With icon labels" @@ of_dom tabs##.root__ in
  let text_section = let items = List.map (fun x ->
                                     Tabs.Tab.create ~content:(`Text ("Tab " ^ (string_of_int x))) ())
                                          (CCList.range 0 3) in
                     let tabs = Tabs.Tab_bar.create ~_type:`Text ~content:items () |> Tabs.Tab_bar.attach in
                     Dom_html.addEventListener
                       tabs##.root__
                       Tabs.Tab.events.selected
                       (Dom_html.handler (fun e ->
                            print_endline ("Tab:selected. Left offset: " ^ (e##.detail_##.tab_##.computedLeft_
                                                                            |> Js.float_of_number
                                                                            |> string_of_float));
                            Js._false))
                       Js._false |> ignore;
                     Dom_html.addEventListener
                       tabs##.root__
                       Tabs.Tab_bar.events.change
                       (Dom_html.handler (fun e ->
                            print_endline ("TabBar:change " ^ (e##.detail_##.activeTabIndex_
                                                               |> Js.float_of_number
                                                               |> int_of_float
                                                               |> string_of_int));
                            Js._false))
                       Js._false |> ignore;
                     subsection "With text labels" @@ of_dom tabs##.root__ in
  let it_section = let items = List.map (fun x ->
                                   Tabs.Tab.create ~content:(`Text_and_icon ("Tab " ^ string_of_int x,
                                                                             (match x with
                                                                              | 0 -> "pets"
                                                                              | 1 -> "favorite"
                                                                              | 2 -> "grade"
                                                                              | _ -> "room")))
                                                   ())
                                        (CCList.range 0 3) in
                   let tabs = Tabs.Tab_bar.create ~_type:`Text_and_icon
                                                  ~content:items ()
                              |> Tabs.Tab_bar.attach in
                   subsection "With icon and text labels" @@ of_dom tabs##.root__ in
  let scroll_section = let tab_items = List.map (fun x ->
                                           Tabs.Tab.create ~content:(`Text ("Tab " ^ (string_of_int x))) ())
                                                (CCList.range 0 15) in
                       let tabs = Tabs.Tab_bar.create ~classes:[Tabs.Scroller.scroll_frame_tabs_class]
                                                      ~_type:`Text
                                                      ~content:tab_items () in
                       let scroller = Tabs.Scroller.create ~tabs ()
                                      |> Tabs.Scroller.attach in
                       subsection "With scroller" @@ of_dom scroller##.root__ in
  demo_section "Tabs" [ text_section; icon_section; it_section; scroll_section ]

(* let snackbar_demo () =
 *   let snackbar = Snackbar.create () |> Snackbar.attach in
 *   let aligned  = Snackbar.create ~start_aligned:true () |> Snackbar.attach in
 *   let snackbar_btn = Button.create
 *                        ~onclick:(fun _ ->
 *                          snackbar##show_ (Snackbar.data_to_js_obj { message = "I am a snackbar"
 *                                                                   ; timeout = None
 *                                                                   ; action  = Some { handler = (fun () -> print_endline "Clicked on snackbar action")
 *                                                                                    ; text    = "Action"}
 *                                                                   ; multiline = None });
 *                          true)
 *                        ~style:"margin:10px"
 *                        ~raised:true
 *                        ~label:"Open snackbar"
 *                        ()in
 *   let aligned_btn =  Button.create
 *                        ~onclick:(fun _ ->
 *                          aligned##show_ (Snackbar.data_to_js_obj { message = "I am a snackbar"
 *                                                                  ; timeout = None
 *                                                                  ; action  = Some { handler = (fun () -> print_endline "Clicked on snackbar action")
 *                                                                                   ; text    = "Action"}
 *                                                                  ; multiline = None });
 *                          true)
 *                        ~style:"margin:10px"
 *                        ~raised:true
 *                        ~label:"Open start-aligned snackbar"
 *                        () in
 *   demo_section "Snackbar" [ of_dom snackbar##.root__
 *                           ; of_dom aligned ##.root__
 *                           ; snackbar_btn
 *                           ; aligned_btn ] *)

(* let textfield_demo () =
 *   let css_label = "css textfield label: " in
 *   let css_textfield = Textfield.create ~placeholder:"placeholder"
 *                                        ~input_id:"demo-css-textfield"
 *                                        () in
 *   let css_sect = subsection "CSS only textfield" @@ of_dom
 *                                                       (new Form_field.t
 *                                                            ~label:css_label
 *                                                            ~input:(Tyxml_js.To_dom.of_element css_textfield)
 *                                                            ~align_end:true
 *                                                            ())#root in
 *   let js_textfield = Textfield.create ~label:"js textfield label"
 *                                       ~input_id:"demo-js-textfield"
 *                                       ~help_text_id:"demo-js-textfield-hint"
 *                                       ~required:true
 *                                       ()
 *                      |> Textfield.attach in
 *   let js_help_text = Textfield.Help_text.create ~id:"demo-js-textfield-hint"
 *                                                 ~text:"This field must not be empty"
 *                                                 ~validation:true
 *                                                 () in
 *   let js_sect = subsection "JS textfield" @@ Html.div [ of_dom js_textfield##.root__
 *                                                       ; js_help_text ] in
 *   let dense_textfield = Textfield.create ~label:"dense textfield label"
 *                                          ~input_type:`Email
 *                                          ~input_id:"demo-dense-textfield"
 *                                          ~dense:true
 *                                          ()
 *                         |> Textfield.attach in
 *   let dense_help_text = Textfield.Help_text.create ~id:"demo-dense-textfield-hint"
 *                                                    ~text:"Provide valid e-mail"
 *                                                    ~validation:true
 *                                                    () in
 *   let dense_sect = subsection "Dense textfield (with email validation)"
 *                    @@ Html.div [ of_dom dense_textfield##.root__
 *                                ; dense_help_text ] in
 *   let lead_icon_textfield = Textfield.create ~label:"textfield label"
 *                                              ~input_id:"lead-icon-textfield"
 *                                              ~leading_icon:(Textfield.Icon.create ~icon:"event" ())
 *                                              ~box:true
 *                                              ()
 *                             |> Textfield.attach in
 *   let trail_icon_textfield = Textfield.create ~label:"textfield label"
 *                                               ~input_id:"trail-icon-textfield"
 *                                               ~trailing_icon:(Textfield.Icon.create ~icon:"delete" ())
 *                                               ~style:"margin-top: 15px"
 *                                               ~box:true
 *                                               ()
 *                              |> Textfield.attach in
 *   let icon_sect = subsection "With icons" @@ Html.div ~a:([Html.a_style "display: flex;\
 *                                                                          max-width: 300px;\
 *                                                                          flex-direction: column;"])
 *                                                [ of_dom lead_icon_textfield##.root__
 *                                                ; of_dom trail_icon_textfield##.root__
 *                                                ] in
 *   let css_textarea = Textfield.create ~textarea:true
 *                                       ~placeholder:"Enter something"
 *                                       ~rows:8
 *                                       ~cols:40
 *                                       () in
 *   let textarea = Textfield.create ~label:"textarea label"
 *                                   ~input_id:"js-textarea-demo"
 *                                   ~textarea:true
 *                                   ~rows:8
 *                                   ~cols:40
 *                                   ()
 *                  |> Textfield.attach in
 *   let css_textarea_sect = subsection "Textarea (css only)" css_textarea in
 *   let textarea_sect = subsection "Textarea" @@ of_dom textarea##.root__ in
 *   demo_section "Textfield" [ css_sect; js_sect; dense_sect; icon_sect; css_textarea_sect; textarea_sect ] *)

let select_demo () =
  let js_select = let select = new Select.Base.t
                                   ~placeholder:"Pick smth"
                                   ~items:(List.map (fun x -> new Select.Base.item
                                                                  ~id:("index " ^ (string_of_int x))
                                                                  ~text:("Select item " ^ (string_of_int x))
                                                                  ()
                                                              |> (fun i -> if x = 3 then i#disable; i))
                                                    (CCList.range 0 5))
                                   () in
                  Dom_html.addEventListener
                    select#root
                    Select.Base.events.change
                    (Dom_html.handler (fun e ->
                         print_endline ("Select:change "
                                        ^ (e##.detail_##.selectedIndex |> string_of_int)
                                        ^ ". Value: "
                                        ^ (e##.detail_##.value |> Js.to_string));
                         Js._false))
                    Js._false |> ignore;
                  subsection "Full-fidelity select" @@ of_dom select#root in
  let css_select = let items = (new Select.Pure.Group.t
                                    ~label:"Group"
                                    ~items:(List.map (fun x -> (new Select.Pure.Item.t
                                                                    ~text:("Group item " ^ (string_of_int x))
                                                                    ~disabled:(x = 1)
                                                                    ())#root)
                                                     (CCList.range 0 2))
                                    ()) (* :: (List.map (fun x -> (new Select.Pure.Item.t
                                         *                             ~text:("Select item " ^ (string_of_int x))
                                         *                             ~disabled:(x = 4)
                                         *                             ())#root)
                                         *              (CCList.range 0 5)) *) in
                   let select = new Select.Pure.t ~items:[items] () in
                   subsection "CSS-only select" @@ of_dom select#root in
  let multi = let items = (Select.Multi.Item.create_group
                             ~label:"Group 1"
                             ~items:(List.map (fun x -> Select.Multi.Item.create
                                                          ~text:("Group item " ^ (string_of_int x))
                                                          ())
                                              (CCList.range 0 2))
                             ())
                          :: (Select.Multi.Item.create_divider ())
                          :: (Select.Multi.Item.create_group
                             ~label:"Group 2"
                             ~items:(List.map (fun x -> Select.Multi.Item.create
                                                          ~text:("Group item " ^ (string_of_int x))
                                                          ())
                                              (CCList.range 0 2))
                             ())
                          :: [] in
              let select = Select.Multi.create ~items ~size:6 () in
              subsection "CSS-only multi select" select in
  demo_section "Select" [ js_select; css_select; multi ]

let toolbar_demo (drawer : Drawer.Persistent.t Js.t) () =
  let last_row = Toolbar.Row.create
                   ~content:[ Toolbar.Row.Section.create
                                ~align:`Start
                                ~content:[ Html.i ~a:[Html.a_class [ "material-icons"
                                                                   ; Toolbar.Row.Section.icon_class]
                                                     ; Html.a_onclick (fun _ -> if drawer##.open_ |> Js.to_bool
                                                                                then drawer##.open_ := Js._false
                                                                                else drawer##.open_ := Js._true
                                                                              ; true)]
                                                  [Html.pcdata "menu"]
                                         ; Toolbar.Row.Section.create_title ~title:"Widgets demo page" () ]
                                ()
                            ; Toolbar.Row.Section.create
                                ~align:`End
                                ~content:[Html.i ~a:[Html.a_class [ "material-icons"
                                                                  ; Toolbar.Row.Section.icon_class]]
                                                 [Html.pcdata "favorite"]]
                                ()
                            ]
                   () in
  let toolbar = Toolbar.create ~content:[ last_row ]
                               ~id:"toolbar"
                               ~waterfall:true
                               ~flexible:true
                               ~fixed:true
                               () in
  To_dom.of_element toolbar

let drawer_demo () =
  Drawer.Temporary.create ~content:[Drawer.Temporary.Toolbar_spacer.create ~content:[Html.pcdata "Demo"]
                                                                           ()]
                          ()
  |> Drawer.Temporary.attach

let chart_demo () =
  (* Chartjs.typ_to_string Chartjs.Line *)
  Chartjs.create_canvas () |> Tyxml_js.To_dom.of_canvas

let add_demos demos =
  Html.div ~a:[ Html.a_id "demo-div"
              (* ; Html.a_style "display: inline-flex;\ *)
              (*                 flex-direction: column;\ *)
              (*                 flex-grow: 1;\ *)
              (*                 height: 100%;\ *)
              (*                 box-sizing: border-box;" *) ]
           @@ CCList.map (fun x -> Of_dom.of_element (x :> Dom_html.element Js.t)) demos
  |> To_dom.of_element

let onload _ =
  let doc = Dom_html.document in
  let body = doc##.body in
  let drawer  = drawer_demo () in
  let toolbar = toolbar_demo drawer () in
  let canvas = chart_demo () in
  let demos = add_demos [ button_demo ()
                        ; Tyxml_js.Html.div ~a:[ Html.a_style "max-width:700px"]
                                            [canvas |> Tyxml_js.Of_dom.of_canvas]
                          |> Tyxml_js.To_dom.of_element
                        ; fab_demo ()
                        ; radio_demo ()
                        ; checkbox_demo ()
                        ; switch_demo ()
                        ; toggle_demo ()
                        ; select_demo ()
                        (* ; textfield_demo () *)
                        ; card_demo ()
                        ; slider_demo ()
                        ; grid_list_demo ()
                        ; ripple_demo ()
                        ; layout_grid_demo ()
                        ; dialog_demo ()
                        ; list_demo ()
                        ; menu_demo ()
                        (* ; snackbar_demo () *)
                        ; linear_progress_demo ()
                        ; tabs_demo ()
                        ] in
  Dom.appendChild body toolbar;
  Dom.appendChild body drawer##.root__;
  Dom.appendChild body demos;
  Dom.appendChild body toolbar;
  let js_toolbar = Toolbar.attach toolbar in
  js_toolbar##.fixedAdjustElement_ := demos;
  let open Chartjs.Line in
  Random.init (Unix.time () |> int_of_float);
  let data = Data.to_obj ~datasets:[ Data.Dataset.to_obj ~label:"My data 1"
                                                         ~fill:(Bool false)
                                                         ~border_color:"rgba(255,0,0,1)"
                                                         ~data:(Points (List.map
                                                                          (fun x -> ({ x = (float_of_int x)
                                                                                     ; y = Random.float 10.
                                                                                     } : Data.Dataset.xy))
                                                                          (CCList.range 0 20))
                                                                : Data.Dataset.data)
                                                         ()
                                   ; Data.Dataset.to_obj ~label:"My data 2"
                                                         ~fill:(Bool false)
                                                         ~border_color:"rgba(0,255,0,1)"
                                                         ~data:(Points (List.map
                                                                          (fun x -> ({ x = (float_of_int x)
                                                                                     ; y = Random.float 50.
                                                                                     } : Data.Dataset.xy))
                                                                          (CCList.range 0 20))
                                                                : Data.Dataset.data)
                                                         ()
                                   ]
                         ~labels:(CCList.range 0 20 |> List.map string_of_int)
                         () in
  let open Chartjs in
  let _ = Chartjs.Line.attach
             ~data
             ~options:(Options.to_obj
                         ~on_hover:(fun e (a:'a Js.js_array Js.t) ->
                           print_endline ("hover! type: "
                                          ^ (Js.to_string @@ e##._type)
                                          ^ ", array length: "
                                          ^ (string_of_int @@ a##.length)))
                         ~on_click:(fun e (a:'a Js.js_array Js.t) ->
                           print_endline ("click! type: "
                                          ^ (Js.to_string @@ e##._type)
                                          ^ ", array length: "
                                          ^ (string_of_int @@ a##.length)))
                         ~hover:(Options.Hover.to_obj ~mode:Index
                                                      ~intersect:false
                                                      ())
                         ~tooltips:(Options.Tooltip.to_obj ~mode:Index
                                                           ~intersect:false
                                                           ())
                         ~scales:(Options.Axes.to_obj
                                    ~x_axes:[ Options.Axes.Cartesian.Linear.to_obj
                                                ~scale_label:(Options.Axes.Scale_label.to_obj
                                                                ~display:true
                                                                ~label_string:"My x axis"
                                                                ())
                                                ()
                                            ]
                                    ())
                         ())
             canvas in
  (* Dom.appendChild body (Button.create ~label:"Get image" *)
  (*                                     ~onclick:(fun _ -> ch##toBase64Image () *)
  (*                                                        |> Js.to_string *)
  (*                                                        |> print_endline; *)
  (*                                                        true) *)
  (*                                     () *)
  (*                       |> Tyxml_js.To_dom.of_element); *)
  Js._false

let () = Dom_html.addEventListener Dom_html.document
                                   Dom_events.Typ.domContentLoaded
                                   (Dom_html.handler onload)
                                   Js._false
         |> ignore
