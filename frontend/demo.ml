open Containers
open Lwt_react
open Components
open Tyxml_js

let demo_section title content =
  new Expansion_panel.t ~title ~content ()

let subsection name w = Html.div [ Html.h3 ~a:[Html.a_class [Typography.font_to_class Subheading_2]]
                                     [Html.pcdata name]
                                 ; Widget.widget_to_markup w ]
                        |> To_dom.of_element
                        |> Widget.create

let button_demo () =
  let raised     = new Button.t ~label:"raised" ~style:`Raised () in
  let flat       = new Button.t ~label:"flat" () in
  let unelevated = new Button.t ~label:"unelevated" ~style:`Unelevated () in
  let stroked    = new Button.t ~label:"stroked" ~style:`Stroked () in
  let ripple     = new Button.t ~label:"ripple" ~ripple:true () in
  let dense      = new Button.t ~label:"dense" ~dense:true () in
  let compact    = new Button.t ~label:"compact" ~compact:true () in
  let icon       = new Button.t ~label:"icon" ~icon:"favorite" () in
  let box        = new Box.t ~widgets:[raised;flat;unelevated;stroked;ripple;dense;compact;icon] () in
  box#set_gap 20;
  box#set_align_items `Start;
  demo_section "Button" [box]

let fab_demo () =
  let fab    = new Fab.t ~icon:"favorite" () in
  let mini   = new Fab.t ~mini:true ~icon:"favorite" () in
  let ripple = new Fab.t ~ripple:true ~icon:"favorite" () in
  let box    = new Box.t ~widgets:[ subsection "General" fab
                                  ; subsection "Mini" mini
                                  ; subsection "Ripple" ripple
                                  ]
                   ()
  in
  demo_section "FAB" [box]

let radio_demo () =
  let radio1 = new Radio.t ~name:"radio" ~value:() () in
  let radio2 = new Radio.t ~name:"radio" ~value:() () in
  let radio3 = new Radio.t ~name:"radio" ~value:() () in
  demo_section "Radio button" [ radio1; radio2; radio3 ]

let checkbox_demo () =
  let checkbox     = new Checkbox.t ~input_id:"checkbox-demo" () in
  let css_checkbox = new Checkbox.t ~ripple:false () in
  let btn          = new Button.t ~label:"toggle indeterminate" () in
  let form_field   = new Form_field.t ~label:"checkbox label" ~input:checkbox () in
  React.E.map (fun _ -> checkbox#set_indeterminate @@ not checkbox#get_indeterminate;
                        css_checkbox#set_indeterminate @@ not css_checkbox#get_indeterminate)
    btn#e_click |> ignore;
  demo_section "Checkbox" [ (subsection "Checkbox (css only)" css_checkbox)#widget
                          ; (subsection "Checkbox with label" form_field)#widget
                          ; btn#widget ]

let switch_demo () =
  let switch   = new Switch.t ~input_id:"demo-switch" () in
  let form     = new Form_field.t ~label:"switch label" ~input:switch () in
  React.S.map (fun x -> print_endline @@ "Switch is " ^ (if x then "on" else "off")) switch#s_state |> ignore;
  demo_section "Switch" [ subsection "Switch" @@ new Switch.t (); subsection "Switch with label" form ]

let toggle_demo () =
  let toggle = new Icon_toggle.t
                 ~on_data:{ icon = "favorite"; label = None; css_class = None }
                 ~off_data:{ icon = "favorite_border"; label = None; css_class = None }
                 () in
  React.S.map (fun x -> print_endline @@ "Icon toggle is " ^ (if x then "on" else "off")) toggle#s_state |> ignore;
  demo_section "Icon toggle" [ toggle ]

let card_demo () =
  let media = new Card.Media.t ~widgets:[] () in
  let url = "url(\"https://maxcdn.icons8.com/app/uploads/2016/03/material-1-1000x563.jpg\")" in
  media#style##.backgroundImage := Js.string url;
  (Js.Unsafe.coerce media#style)##.backgroundSize := Js.string "cover";
  media#style##.backgroundRepeat := Js.string "no-repeat";
  media#style##.height := Js.string "12.313rem";
  let title    = new Card.Title.t ~large:true ~title:"Demo card title" () in
  let subtitle = new Card.Subtitle.t ~subtitle:"Subtitle" () in
  let primary  = new Card.Primary.t ~widgets:[ title#widget; subtitle#widget ] () in
  let text     = new Card.Supporting_text.t ~text:"Supporting text" () in
  let actions  = new Card.Actions.t ~widgets:[ new Button.t ~compact:true ~label:"action 1" ()
                                             ; new Button.t ~compact:true ~label:"action 2" () ] () in
  let card = new Card.t ~sections:[ `Media media; `Primary primary; `Text text; `Actions actions ] () in
  card#style##.width := Js.string "320px";
  demo_section "Card" [ card ]

let slider_demo () =
  let listen elt name =
    React.S.map (fun x -> Printf.printf "Input on %s slider, value = %f\n" name x) elt#s_input |> ignore;
    React.S.map (fun x -> Printf.printf "Value on %s slider, value = %f\n" name x) elt#s_value |> ignore in
  let continuous   = new Slider.t () in
  let discrete     = new Slider.t ~discrete:true () in
  let with_markers = new Slider.t ~discrete:true ~markers:true () in
  let disabled     = new Slider.t () in
  disabled#set_disabled true;
  listen continuous "continuous";
  listen discrete "discrete";
  listen with_markers "markered";
  Dom_html.setTimeout (fun () -> continuous#layout; discrete#layout; with_markers#layout) 100. |> ignore;
  demo_section "Slider" [ subsection "Continuous slider" continuous
                        ; subsection "Discrete slider" discrete
                        ; subsection "Discrete slider with markers" with_markers
                        ; subsection "Disabled slider" disabled ]

let grid_list_demo () =
  let tiles = List.map (fun x -> new Grid_list.Tile.t
                                   ~src:"https://cs5-3.4pda.to/5290239.png"
                                   ~title:("My tile " ^ (string_of_int x))
                                   ~support_text:"Some text here"
                                   ())
                (List.range 0 4) in
  let grid  = new Grid_list.t ~tiles () in
  demo_section "Grid list" [ grid ]

let ripple_demo () =
  let bounded = Widget.create (Html.div ~a:[Html.a_style "height: 200px; width: 200px; margin: 20px"] []
                               |> To_dom.of_element) in
  Elevation.set_elevation bounded 5; Ripple.attach bounded |> ignore;
  let unbounded      = new Icon.Font.t ~icon:"favorite" () in
  Ripple.attach unbounded |> ignore; Ripple.set_unbounded unbounded;
  demo_section "Ripple" [ subsection "Bounded ripple. Click me!" bounded
                        ; subsection "Unbounded ripple. Click me!" unbounded]

let layout_grid_demo () =
  let cells = List.map (fun x -> let w = Html.div ~a:[Html.a_style "box-sizing: border-box;\
                                                                    background-color: #666666;\
                                                                    height: 200px;\
                                                                    padding: 8px;\
                                                                    color: white;\
                                                                    font-size: 1.5em;"]
                                           [Html.pcdata ( "id=" ^ (string_of_int x)
                                                          ^ "\nspan="
                                                          ^ (string_of_int (if x = 3
                                                                            then 2
                                                                            else 1)))]
                                         |> Tyxml_js.To_dom.of_element
                                         |> Widget.create in
                                 new Layout_grid.Cell.t ~widgets:[w] ()
                                 |> (fun x -> x#set_span 1; x))
                (List.range 0 15) in
  let btn2 = new Button.t ~label:"set span 1" () in
  let btn4 = new Button.t ~label:"set span 2" () in
  React.E.map (fun _ -> (List.get_at_idx_exn 4 cells)#set_span 1) btn2#e_click |> ignore;
  React.E.map (fun _ -> (List.get_at_idx_exn 4 cells)#set_span 2) btn4#e_click |> ignore;
  let layout_grid = new Layout_grid.t ~cells () in
  demo_section "Layout grid" [ layout_grid#widget; btn2#widget; btn4#widget ]

let dialog_demo () =
  let dialog = new Dialog.t
                 ~title:"This is dialog"
                 ~content:(`String "Dialog body")
                 ~actions:[ new Dialog.Action.t ~typ:`Decline ~label:"Decline" ()
                          ; new Dialog.Action.t ~typ:`Accept  ~label:"Accept" ()
                 ]
                 () in
  let button = new Button.t ~label:"show dialog" () in
  React.E.map (fun _ -> Lwt.bind dialog#show_await
                          (function
                           | `Accept -> print_endline "Dialog accepted"; Lwt.return ()
                           | `Cancel -> print_endline "Dialog cancelled"; Lwt.return ()))
    button#e_click |> ignore;
  demo_section "Dialog" [ dialog#widget; button#widget ]

let list_demo () =
  let items = List.map (fun x -> if x = 3
                                 then `Divider (new Item_list.Divider.t ())
                                 else `Item (new Item_list.Item.t
                                               ~text:("List item " ^ (string_of_int x))
                                               ~secondary_text:"some subtext here"
                                               ~start_detail:(new Avatar.Letter.t ~text:"A" ())
                                               ~ripple:true
                                               ()))
                (List.range 0 5) in
  let list = new Item_list.t ~avatar:true ~items () in
  list#style##.maxWidth := Js.string "400px";
  let list1 = new Item_list.t
                ~items:[ `Item (new Item_list.Item.t ~text:"Item 1" ~secondary_text:"Subtext" ())
                       ; `Item (new Item_list.Item.t ~text:"Item 2" ~secondary_text:"Subtext" ())
                       ; `Item (new Item_list.Item.t ~text:"Item 3" ~secondary_text:"Subtext" ())
                ]
                () in
  let list2 = new Item_list.t
                ~items:[ `Item (new Item_list.Item.t ~text:"Item 1" ~secondary_text:"Subtext" ())
                       ; `Item (new Item_list.Item.t ~text:"Item 2" ~secondary_text:"Subtext" ())
                       ; `Item (new Item_list.Item.t ~text:"Item 3" ~secondary_text:"Subtext" ())
                ]
                () in
  let group = new Item_list.List_group.t
                ~content:[ { subheader = Some "Group 1"; list = list1 }
                         ; { subheader = Some "Group 2"; list = list2 }
                ]
                () in
  group#style##.maxWidth := Js.string "400px";
  demo_section "List" [ list#widget; group#widget ]

let tree_demo () =
  let item x = new Tree.Item.t
                 ~text:("Item " ^ string_of_int x)
                 ~nested:(new Tree.t
                            ~items:[ new Tree.Item.t ~text:"Item 0" ()
                                   ; new Tree.Item.t ~text:"Item 1" ()
                                   ; new Tree.Item.t ~text:"Item 2" () ]
                            ())
                 () in
  let tree = new Tree.t
               ~items:(List.map (fun x -> item x) (List.range 0 5))
               () in
  tree#style##.maxWidth := Js.string "400px";
  demo_section "Tree" [ tree ]

let menu_demo () =
  let items    = List.map (fun x -> if x != 2
                                    then `Item (new Menu.Item.t ~text:("Menu item " ^ (string_of_int x)) ())
                                    else `Divider (new Menu.Divider.t ()))
                   (List.range 0 5) in
  let anchor  = new Button.t ~label:"Open menu" () in
  anchor#style##.marginBottom := Js.string "50px";
  let menu    = new Menu.t ~items () in
  let wrapper = new Menu.Wrapper.t ~menu ~anchor () in
  menu#set_dense true;
  let icon_anchor = new Icon.Font.t ~icon:"more_horiz" () in
  let icon_menu   = new Menu.t
                      ~items:[ `Item (new Menu.Item.t ~text:"Item 1" ())
                             ; `Item (new Menu.Item.t ~text:"Item 2" ())
                             ; `Item (new Menu.Item.t ~text:"Item 3" ()) ]
                      () in
  let icon_wrapper = new Menu.Wrapper.t ~menu:icon_menu ~anchor:icon_anchor () in
  React.E.map (fun _ -> menu#show) anchor#e_click      |> ignore;
  Dom_html.addEventListener icon_anchor#root
    Dom_events.Typ.click
    (Dom_html.handler (fun _ -> icon_menu#show; Js._false))
    Js._false
  |> ignore;
  Dom_html.addEventListener menu#root
    Menu.events.selected
    (Dom_html.handler (fun d ->
         print_endline ("Selected menu item is " ^ (d##.detail##.index
                                                    |> string_of_int));
         Js._false))
    Js._false
  |> ignore;
  Dom_html.addEventListener menu#root
    Menu.events.cancel
    (Dom_html.handler (fun _ -> print_endline "Menu cancelled"; Js._false))
    Js._false
  |> ignore;
  demo_section "Menu" [ wrapper#widget; icon_wrapper#widget ]

let linear_progress_demo () =
  let linear_progress = new Linear_progress.t () in
  linear_progress#set_indeterminate true;
  let ind_btn   = new Button.t ~label:"indeterminate" () in
  let det_btn   = new Button.t ~label:"determinate" () in
  let pgs0_btn  = new Button.t ~label:"progress 0" () in
  let pgs20_btn = new Button.t ~label:"progress 20" () in
  let pgs60_btn = new Button.t ~label:"progress 60" () in
  let buf10_btn = new Button.t ~label:"buffer 10" () in
  let buf30_btn = new Button.t ~label:"buffer 30" () in
  let buf70_btn = new Button.t ~label:"buffer 70" () in
  let open_btn  = new Button.t ~label:"open" () in
  let close_btn = new Button.t ~label:"close" () in
  React.E.map (fun _ -> linear_progress#set_indeterminate true) ind_btn#e_click  |> ignore;
  React.E.map (fun _ -> linear_progress#set_indeterminate false) det_btn#e_click |> ignore;
  React.E.map (fun _ -> linear_progress#set_progress 0.) pgs0_btn#e_click        |> ignore;
  React.E.map (fun _ -> linear_progress#set_progress 0.2) pgs20_btn#e_click      |> ignore;
  React.E.map (fun _ -> linear_progress#set_progress 0.6) pgs60_btn#e_click      |> ignore;
  React.E.map (fun _ -> linear_progress#set_buffer 0.1) buf10_btn#e_click        |> ignore;
  React.E.map (fun _ -> linear_progress#set_buffer 0.3) buf30_btn#e_click        |> ignore;
  React.E.map (fun _ -> linear_progress#set_buffer 0.7) buf70_btn#e_click        |> ignore;
  React.E.map (fun _ -> linear_progress#show) open_btn#e_click                   |> ignore;
  React.E.map (fun _ -> linear_progress#hide) close_btn#e_click                  |> ignore;
  let cells = List.map (fun x -> new Layout_grid.Cell.t ~widgets:[x] ()
                                 |> (fun x -> x#set_span 12; x))
                [ind_btn  ; det_btn  ; pgs0_btn ; pgs20_btn; pgs60_btn;
                 buf10_btn; buf30_btn; buf70_btn; open_btn ; close_btn ] in
  let btn_grid = new Layout_grid.t ~cells () in
  demo_section "Linear progress" [ btn_grid#widget; linear_progress#widget ]

let tabs_demo () =
  let open Components.Tabs in
  let idx       = new Textfield.t ~input_type:(Integer None) ~label:"index" () in
  let add       = new Button.t ~label:"add" () in
  let remove    = new Button.t ~label:"remove" () in
  let icon_bar  = [ { content = `Icon ("pets", None)     ; href = Some "#1"; disabled = false; value = () }
                  ; { content = `Icon ("favorite", None) ; href = Some "#2"; disabled = false; value = () }
                  ; { content = `Icon ("grade", None)    ; href = Some "#3"; disabled = false; value = () }
                  ; { content = `Icon ("room", None)     ; href = Some "#4"; disabled = false; value = () }
                  ] |> (fun tabs -> new Tabs.Tab_bar.t ~tabs ()) in
  let text_bar  = List.map (fun x -> { content  = `Text ("Tab " ^ (string_of_int x))
                                     ; href     = None
                                     ; disabled = if x = 2 then true else false
                                     ; value    = ()})
                    (List.range 0 3)
                  |> (fun tabs -> new Tabs.Tab_bar.t ~tabs ()) in
  let both_bar  = [ { content = `Text_and_icon ("Tab 0", "pets");     href = None; disabled = false; value = () }
                  ; { content = `Text_and_icon ("Tab 1", "favorite"); href = None; disabled = false; value = () }
                  ; { content = `Text_and_icon ("Tab 2", "grade");    href = None; disabled = true ; value = () }
                  ; { content = `Text_and_icon ("Tab 3", "room");     href = None; disabled = false; value = () }
                  ] |> (fun tabs -> new Tabs.Tab_bar.t ~tabs ()) in
  let scrl_bar  = List.map (fun x -> { content = `Text ("Tab " ^ (string_of_int x))
                                     ; href = None
                                     ; disabled = false
                                     ; value = () })
                    (List.range 0 15)
                  |> (fun tabs -> new Tabs.Scroller.t ~tabs ()) in
  React.E.map (fun _ ->
      let len  = List.length text_bar#tabs in
      let name = Printf.sprintf "Tab %d" len in
      match React.S.value idx#s_input with
      | Some idx -> text_bar#insert_tab_at_index idx { content = `Text name
                                                     ; href = None
                                                     ; disabled = false
                                                     ; value = ()
                      }
      | None     -> text_bar#append_tab { content = `Text name; href = None; disabled = false; value = () })
    add#e_click
  |> ignore;
  React.E.map (fun _ ->
      match React.S.value idx#s_input with
      | Some idx -> text_bar#remove_tab_at_index idx |> ignore
      | None     -> ())
    remove#e_click
  |> ignore;
  demo_section "Tabs" [ (subsection "With icon labels" icon_bar)#widget
                      ; (subsection "With text labels" text_bar)#widget
                      ; idx#widget
                      ; add#widget
                      ; remove#widget
                      ; (subsection "With icon and text labels" both_bar)#widget
                      (* ; (subsection "With scroller" scrl_bar)#widget  *)]

let snackbar_demo () =
  let snackbar = new Snackbar.t
                   ~message:"I am a snackbar"
                   ~action:{ handler = (fun () -> print_endline "Clicked on snackbar action")
                           ; text    = "Action"}
                   () in
  let aligned  = new Snackbar.t
                   ~start_aligned:true
                   ~message:"I am a snackbar"
                   ~action:{ handler = (fun () -> print_endline "Clicked on snackbar action")
                           ; text    = "Action"}
                   () in
  let snackbar_btn = new Button.t ~label:"Open snackbar" () in
  let aligned_btn  = new Button.t ~label:"Open start-aligned snackbar" () in
  React.E.map (fun _ -> snackbar#show) snackbar_btn#e_click |> ignore;
  React.E.map (fun _ -> aligned#show) aligned_btn#e_click |> ignore;
  Dom.appendChild Dom_html.document##.body snackbar#root;
  Dom.appendChild Dom_html.document##.body aligned#root;
  demo_section "Snackbar" [ snackbar_btn; aligned_btn ]

let textfield_demo () =
  (* CSS only textbox *)
  let css      = new Textfield.Pure.t ~placeholder:"placeholder" ~input_id:"demo-css-textfield" () in
  let css_form = new Form_field.t ~label:"css textfield label: " ~input:css ~align_end:true () in
  (* Full-featured js textbox *)
  let js       = new Textfield.t
                   ~input_type:Widget.Text 
                   ~label:"js textfield label"
                   ~help_text:{ validation = true
                              ; persistent = false
                              ; text       = Some "This field must not be empty"
                   }
                   () in
  js#set_required true;
  (* Dense js textbox with *)
  let dense    = new Textfield.t
                   ~label:"dense textfield label"
                   ~input_type:Widget.Email
                   ~help_text:{ validation = true
                              ; persistent = false
                              ; text       = Some "Provide valid e-mail"
                   }
                   () in
  dense#set_dense true;
  (* Textboxes with icons *)
  let lead_icon  = new Textfield.t
                     ~input_type:Widget.Text
                     ~label:"textfield label"
                     ~icon:{ icon      = "event"
                           ; clickable = false
                           ; pos       = `Leading
                     }
                     () in
  let trail_icon = new Textfield.t
                     ~input_type:Widget.Text
                     ~label:"textfield label"
                     ~icon:{ icon      = "delete"
                           ; clickable = false
                           ; pos       = `Trailing
                     }
                     () in
  (* Textareas *)
  let css_textarea      = new Textarea.Pure.t ~placeholder:"Enter something" ~rows:8 ~cols:40 () in
  let textarea          = new Textarea.t ~label:"textarea label" ~rows:8 ~cols:40 () in
  demo_section "Textfield" [ subsection "CSS only textfield" css_form
                           ; subsection "JS textfield" js
                           ; subsection "Dense textfield (with email validation)" dense
                           ; subsection "With leading icon" lead_icon
                           ; subsection "With trailing icon" trail_icon
                           ; subsection "Textarea (css only)" css_textarea
                           ; subsection "Textarea" textarea ]

let select_demo () =
  let js      = new Select.Base.t
                  ~label:"Pick smth"
                  ~items:(List.map (fun x -> new Select.Base.Item.t
                                               ~id:("index " ^ (string_of_int x))
                                               ~text:("Select item " ^ (string_of_int x))
                                               ~value:()
                                               ())
                            (List.range 0 5))
                  () in
  js#set_dense true;
  let pure    = new Select.Pure.t
                  ~items:[ `Group (new Select.Pure.Group.t
                                     ~label:"Group 1"
                                     ~items:[ new Select.Pure.Item.t ~text:"Item 1" ()
                                            ; new Select.Pure.Item.t ~text:"Item 2" ()
                                            ; new Select.Pure.Item.t ~text:"Item 3" ()]
                                     ())
                         ; `Item (new Select.Pure.Item.t ~text:"Item 1" ())
                         ; `Item (new Select.Pure.Item.t ~text:"Item 2" ())
                         ; `Item (new Select.Pure.Item.t ~text:"Item 3" ())
                  ]
                  () in
  let multi = [ `Group (new Select.Multi.Group.t
                          ~label:"Group 1"
                          ~items:(List.map (fun x -> let text = "Group item " ^ (string_of_int x) in
                                                     new Select.Multi.Item.t ~text ())
                                    (List.range 0 2))
                          ())
              ; `Divider (new Select.Multi.Divider.t ())
              ; `Group (new Select.Multi.Group.t
                          ~label:"Group 2"
                          ~items:(List.map (fun x -> let text = "Group item " ^ (string_of_int x) in
                                                     new Select.Multi.Item.t ~text ())
                                    (List.range 0 2))
                          ())
              ; `Divider (new Select.Multi.Divider.t ())
              ; `Item (new Select.Multi.Item.t ~text:"Item 1" ())
              ; `Item (new Select.Multi.Item.t ~text:"Item 2" ()) ]
              |> (fun items -> new Select.Multi.t ~items ~size:12 ()) in
  demo_section "Select" [ subsection "Full-fidelity select" js
                        ; subsection "Pure (css-only) select" pure
                        ; subsection "CSS-only multi select" multi ]

let toolbar_demo (drawer : Drawer.Persistent.t Js.t) () =
  let icon = Html.i ~a:[Html.a_class [ "material-icons"; Markup.Toolbar.Row.Section.icon_class]
                      ; Html.a_onclick (fun _ -> if drawer##.open_ |> Js.to_bool
                                                 then drawer##.open_ := Js._false
                                                 else drawer##.open_ := Js._true
                                               ; true)]
               [Html.pcdata "menu"]
             |> To_dom.of_i
             |> Widget.create in
  let title = new Toolbar.Row.Section.Title.t ~title:"Widgets demo page" () in
  let section_start = new Toolbar.Row.Section.t ~widgets:[ icon#widget; title#widget ] () in
  section_start#set_align `Start;
  let icon_menu = new Menu.t
                    ~open_from:`Top_right
                    ~items:[ `Item (new Menu.Item.t ~text:"Item 1" ())
                           ; `Item (new Menu.Item.t ~text:"Item 2" ())
                           ; `Item (new Menu.Item.t ~text:"Item 3" ()) ]
                    () in
  let end_icon = Html.i ~a:[Html.a_class [ "material-icons"; Markup.Toolbar.Row.Section.icon_class]]
                   [Html.pcdata "favorite"]
                 |> To_dom.of_i
                 |> Widget.create in
  Menu.inject ~anchor:end_icon ~menu:icon_menu;
  Dom_html.addEventListener end_icon#root
    Dom_events.Typ.click
    (Dom_html.handler (fun _ -> icon_menu#show; Js._false))
    Js._false
  |> ignore;
  let section_end = new Toolbar.Row.Section.t ~widgets:[end_icon] () in
  section_end#set_align `End;
  let row = new Toolbar.Row.t ~sections:[ section_start; section_end ] () in
  let toolbar = new Toolbar.t ~rows:[ row ] () in
  toolbar#root

let elevation_demo () =
  let d = Widget.create (Html.div ~a:[Html.a_style "height: 200px; width: 200px; margin: 20px"]
                           []
                         |> To_dom.of_element) in
  let btn2 = new Button.t ~label:"elevation 2" () in
  let btn8 = new Button.t ~label:"elevation 8" () in
  React.E.map (fun _ -> Elevation.set_elevation d 2) btn2#e_click |> ignore;
  React.E.map (fun _ -> Elevation.set_elevation d 8) btn8#e_click |> ignore;
  demo_section "Elevation" [ d#widget; btn2#widget; btn8#widget ]

let drawer_demo () =
  Drawer.Temporary.create ~content:[Drawer.Temporary.Toolbar_spacer.create ~content:[Html.pcdata "Demo"]
                                      ()] ()
  |> Drawer.Temporary.attach

let table_demo () =
  let header = new Table.Header.t ~content:[ Text "col 1"; Text "col 2"; Text "col 3"] () in
  let table = new Table.t ~header ~content:[ [ Text "cell 1 1"; Text "cell 1 2"; Text "cell 1 3" ]
                                           ; [ Text "cell 2 1"; Text "cell 2 2"; Text "cell 2 3" ]
                                           ; [ Text "cell 3 1"; Text "cell 3 2"; Text "cell 3 3" ]
                ] () in
  table#style##.maxWidth := Js.string "600px";
  table#add_class @@ Elevation.get_elevation_class 2;
  demo_section "Table" [ table#widget ]

let chart_demo () =
  let range = 10 in
  let x = ref 40 in
  Random.init (Unix.time () |> int_of_float);
  let open Chartjs.Line in
  let data = [ { data = (List.map (fun x -> { x ; y = Random.run (Random.int range) }) (List.range_by ~step:2 0 !x))
               ; label = "Dataset 1"
               }
             ; { data = (List.map (fun x -> { x ; y = Random.run (Random.int range) }) (List.range_by ~step:2 0 !x))
               ; label = "Dataset 2"
               }
             ] in
  let config = new Config.t
                 ~x_axis:(Linear ("my-x-axis",Bottom,Int,Some !x))
                 ~y_axis:(Linear ("my-y-axis",Left,Int,None))
                 ~data
                 () in
  config#options#hover#set_mode Index;
  config#options#hover#set_intersect true;
  config#options#tooltip#set_mode Index;
  config#options#tooltip#set_intersect false;
  config#options#y_axis#ticks#set_suggested_max range;
  config#options#x_axis#scale_label#set_label_string "x axis";
  config#options#x_axis#scale_label#set_display true;
  config#options#elements#line#set_border_width 3;
  List.iter (fun x -> if String.equal x#get_label "Dataset 1"
                      then x#set_border_color @@ Color.rgb_of_name (Color.Lime C500)
                      else x#set_border_color @@ Color.rgb_of_name (Color.Pink C500);
                      x#set_cubic_interpolation_mode Monotone;
                      x#set_fill Disabled) config#datasets;
  let update = new Button.t ~label:"update" () in
  let push   = new Button.t ~label:"push" () in
  let push_less = new Button.t ~label:"push less" () in
  let append    = new Button.t ~label:"append" () in
  let chart  = new Chartjs.Line.t ~config () in
  React.E.map (fun _ -> List.iter (fun x -> x#set_point_radius (`Fun (fun _ x -> if x.x mod 2 > 0 then 10 else 5))
                          ) chart#config#datasets;
                        chart#update None)
    update#e_click |> ignore;
  React.E.map (fun _ -> x := !x + 2;
                        List.iter (fun ds -> ds#push { x = !x; y = Random.run (Random.int range) }) chart#config#datasets;
                        chart#update None)
    push#e_click |> ignore;
  React.E.map (fun _ -> List.iter (fun ds -> ds#push { x = !x - 1; y = Random.run (Random.int range) }) chart#config#datasets;
                        chart#update None)
    push_less#e_click |> ignore;
  React.E.map (fun _ -> x := !x + 6;
                        List.iter (fun ds -> ds#append [ { x = !x - 6; y = Random.run (Random.int range) }
                                                       ; { x = !x - 4; y = Random.run (Random.int range) }
                                                       ; { x = !x - 2; y = Random.run (Random.int range) }
                                                       ; { x = !x    ; y = Random.run (Random.int range) } ])
                          chart#config#datasets;
                        chart#update None)
              append#e_click |> ignore;
  let w = Html.div ~a:[ Html.a_style "max-width:700px"] [ Widget.widget_to_markup chart
                                                        ; Widget.widget_to_markup update
                                                        ; Widget.widget_to_markup push
                                                        ; Widget.widget_to_markup push_less
                                                        ; Widget.widget_to_markup append ]
          |> To_dom.of_element
          |> Widget.create
  in
  demo_section "Chart" [w]

let time_chart_demo () =
  let range = 20 in
  Random.init (Unix.time () |> int_of_float);
  let open Chartjs.Line in
  let data = [ { data = []; label = "Dataset 1" }
             ; { data = []; label = "Dataset 2" }
             ] in
  let config = new Config.t
                 ~x_axis:(Time ("my-x-axis",Bottom,Unix,Some 20000L))
                 ~y_axis:(Linear ("my-y-axis",Left,Int,None))
                 ~data
                 () in
  config#options#hover#set_mode Index;
  config#options#hover#set_intersect true;
  config#options#tooltip#set_mode Index;
  config#options#tooltip#set_intersect false;
  config#options#x_axis#scale_label#set_label_string "x axis";
  config#options#x_axis#scale_label#set_display true;
  config#options#x_axis#time#set_min_unit Second;
  config#options#elements#line#set_border_width 3;
  List.iter (fun x -> if String.equal x#get_label "Dataset 1"
                      then x#set_background_color @@ Color.rgb_of_name (Color.Indigo C500)
                      else x#set_background_color @@ Color.rgb_of_name (Color.Amber C500);
                      if String.equal x#get_label "Dataset 1"
                      then x#set_border_color @@ Color.rgb_of_name (Color.Indigo C500)
                      else x#set_border_color @@ Color.rgb_of_name (Color.Amber C500);
                      x#set_cubic_interpolation_mode Monotone;
                      x#set_fill Disabled) config#datasets;
  let chart = new Chartjs.Line.t ~config () in
  let e_update,e_update_push = React.E.create () in
  React.E.map (fun () -> List.iter (fun ds -> ds#push { x = Unix.time () *. 1000. |> Int64.of_float_exn
                                                      ; y = Random.run (Random.int range) }) chart#config#datasets;
                         chart#update None)
    e_update |> ignore;
  Dom_html.window##setInterval (Js.wrap_callback (fun () -> e_update_push () |> ignore)) 1000. |> ignore;
  let w = Html.div ~a:[ Html.a_style "max-width:700px"] [ Widget.widget_to_markup chart ]
          |> To_dom.of_element
          |> Widget.create
  in
  demo_section "Chart (timeline)" [w]

let dynamic_grid_demo () =
  let (props:Dynamic_grid.grid) =
    { rows             = None
    ; cols             = 30
    ; min_col_width    = 1
    ; max_col_width    = None
    ; row_height       = None
    ; vertical_compact = true
    ; items_margin     = None
    } in
  let move () = new Icon_toggle.t
                  ~on_data:{ icon = "open_with"
                           ; label = None
                           ; css_class = None }
                  ~off_data:{ icon = "open_with"
                            ; label = None
                            ; css_class = None }
                  () in
  let items    = [ Dynamic_grid.Item.to_item
                     ~pos:{ x = 0
                          ; y = 0
                          ; w = 10
                          ; h = 10 }
                     ?move_widget:(Some (move())#widget)
                     ~draggable:false
                     ~resizable:false
                     ~value:()
                     ()
                 ; Dynamic_grid.Item.to_item
                     ~pos:{ x = 20
                          ; y = 0
                          ; w = 10
                          ; h = 20 }
                     ?move_widget:(Some (move())#widget)
                     ~value:()
                     ()
                 ]
  in
  let x        = new Textfield.t ~label:"x position" ~input_type:(Widget.Integer None) () in
  let y        = new Textfield.t ~label:"y position" ~input_type:(Widget.Integer None) () in
  let w        = new Textfield.t ~label:"width"      ~input_type:(Widget.Integer None) () in
  let h        = new Textfield.t ~label:"height"     ~input_type:(Widget.Integer None) () in
  let add      = new Button.t ~label:"add" () in
  let add_free = new Button.t ~label:"add free" () in
  let remove   = new Button.t ~label:"remove" () in
  let rem_all  = new Button.t ~label:"remove all" () in
  let grid     = new Dynamic_grid.t ~grid:props ~items () in
  React.E.map (fun e -> let open Lwt.Infix in
                        Dom_html.stopPropagation e;
                        grid#free ?move_widget:(Some (move())#widget)
                          ?width:(React.S.value w#s_input)
                          ?height:(React.S.value h#s_input)
                          ~value:()
                          ~act:Add
                          ()
                        >>= (function
                             | Ok _    -> print_endline "ok"   ; Lwt.return_unit
                             | Error _ -> print_endline "error"; Lwt.return_unit)
                        |> ignore) add_free#e_click
  |> ignore;
  React.E.map (fun e -> let open Lwt.Infix in
                        Dom_html.stopPropagation e;
                        grid#free ~act:Remove ~value:() ()
                        >>= (function
                             | Ok _    -> print_endline "ok"   ; Lwt.return_unit
                             | Error _ -> print_endline "error"; Lwt.return_unit)
                        |> ignore) remove#e_click
  |> ignore;
  React.E.map (fun _ -> grid#remove_all()) rem_all#e_click
  |> ignore;
  React.E.map (fun _ -> match React.S.value x#s_input,React.S.value y#s_input,
                              React.S.value w#s_input,React.S.value h#s_input with
                        | Some x, Some y, Some w, Some h ->
                           grid#add (Dynamic_grid.Item.to_item
                                       ~pos:{ x; y; w; h }
                                       ?move_widget:(Some (move())#widget)
                                       ~value:()
                                       ())
                           |> (function
                               | Ok _    -> print_endline "ok"
                               | Error _ -> ())
                        | _ -> ()) add#e_click |> ignore;
  React.S.map (fun x -> Printf.printf "%d items in grid\n" @@ CCList.length x) grid#s_items |> ignore;
  let sect = demo_section "Dynamic grid" [ grid#widget
                                         ; x#widget
                                         ; y#widget
                                         ; w#widget
                                         ; h#widget
                                         ; add#widget
                                         ; add_free#widget
                                         ; remove#widget
                                         ; rem_all#widget
                                         ]
  in
  let _ = React.S.map (fun x -> if x then grid#layout) sect#s_expanded in
  sect

let expansion_panel_demo () =
  let ep1 = new Expansion_panel.t
                ~title:"Trip name"
                ~details:[ new Box.t ~widgets:[ new Typography.Text.t ~text:"Caribbean cruise" ()
                                              ; new Typography.Text.t ~text:"Second line" ()
                                              ] () ]
                ~content:[]
                () in
  let ep2 = new Expansion_panel.t
                ~title:"Location"
                ~heading_details:[ new Typography.Text.t ~text:"Optional" () ]
                ~details:[ new Typography.Text.t ~text:"Barbados" () ]
                ~content:[ new Typography.Text.t ~text:"This is an expansion panel body text!!!" () ]
                ~actions:[ new Button.t ~label:"Cancel" ()
                         ; new Button.t ~label:"Save" () ]
                () in
  let ep3 = new Expansion_panel.t
                ~title:"Start and end dates"
                ~details:[ new Typography.Text.t ~text:"Start date: Feb 29, 2016" ()
                         ; new Typography.Text.t ~text:"End date: Not set" ()
                         ]
                ~content:[]
                () in
  ep1#add_class (Elevation.get_elevation_class 2);
  ep2#add_class (Elevation.get_elevation_class 2);
  ep3#add_class (Elevation.get_elevation_class 2);
  let box = new Box.t ~widgets:[ep1;ep2;ep3] () in
  demo_section "Expansion panel" [ box ]

let add_demos demos =
  let demos = CCList.sort (fun x y -> CCString.compare x#get_title y#get_title) demos in
  Html.div ~a:[ Html.a_id "demo-div" ]
  @@ CCList.map (fun x -> Of_dom.of_element x#root) demos
  |> To_dom.of_element

let onload _ =
  let ac = Dom_html.getElementById "arbitrary-content" in
  ac##.style##.margin := Js.string "20px";
  (* let doc     = Dom_html.document in
   * let body    = doc##.body in
   * let drawer  = drawer_demo () in
   * let toolbar = toolbar_demo drawer () in *)
  let demos   = add_demos [ expansion_panel_demo ()
                          ; dynamic_grid_demo ()
                          ; table_demo ()
                          ; button_demo ()
                          ; chart_demo ()
                          ; time_chart_demo ()
                          ; fab_demo ()
                          ; radio_demo ()
                          ; checkbox_demo ()
                          ; switch_demo ()
                          ; toggle_demo ()
                          ; elevation_demo ()
                          ; select_demo ()
                          ; textfield_demo ()
                          ; card_demo ()
                          ; slider_demo ()
                          ; grid_list_demo ()
                          ; ripple_demo ()
                          ; layout_grid_demo ()
                          ; dialog_demo ()
                          ; list_demo ()
                          ; tree_demo ()
                          ; menu_demo ()
                          ; snackbar_demo ()
                          ; linear_progress_demo ()
                          ; tabs_demo ()
                          ] in
  (* Dom.appendChild body toolbar;
   * Dom.appendChild body drawer##.root__; *)
  Dom.appendChild ac demos;
  Js._false

let () = Dom_html.addEventListener Dom_html.document
           Dom_events.Typ.domContentLoaded
           (Dom_html.handler onload)
           Js._false
         |> ignore
