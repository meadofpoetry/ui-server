open Containers
open Lwt_react
open Components
open Tyxml_js

let demo_section ?expanded title content =
  new Expansion_panel.t ?expanded ~title ~content ()

let subsection name w = Html.div [ Html.h3 ~a:[Html.a_class [Typography.font_to_class Subheading_2]]
                                     [Html.pcdata name]
                                 ; Widget.to_markup w ]
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
  let box        = new Vbox.t ~widgets:[raised;flat;unelevated;stroked;ripple;dense;compact;icon] () in
  box#set_gap 20;
  box#set_align_items `Start;
  demo_section "Button" [box]

let circular_progress_demo () =
  let indeterminate = new Circular_progress.t ~indeterminate:true () in
  let determinate   = new Circular_progress.t ~indeterminate:false ~max:100. () in
  let slider        = new Slider.t ~min:0.0 ~max:100. ~markers:true () in
  let box           = new Vbox.t ~widgets:[ determinate#widget; slider#widget ] () in
  let _             = React.S.map (fun v -> determinate#set_progress v) slider#s_input in
  let section = demo_section "Circular progress" [ subsection "Indeterminate" indeterminate
                                                 ; subsection "Determinate" box ]
  in
  let _             = React.S.map (fun x -> if x then slider#layout ()) section#s_expanded in
  section

let fab_demo () =
  let fab    = new Fab.t ~icon:"favorite" () in
  let mini   = new Fab.t ~mini:true ~icon:"favorite" () in
  let ripple = new Fab.t ~ripple:true ~icon:"favorite" () in
  let box    = new Vbox.t ~widgets:[ subsection "General" fab
                                  ; subsection "Mini" mini
                                  ; subsection "Ripple" ripple
                                  ]
                   ()
  in
  demo_section "FAB" [box]

let fab_speed_dial_demo () =
  let items = List.map (fun icon -> new Fab.t ~icon ()) ["face"; "add"; "close"] in
  let fab   = new Fab_speed_dial.t ~icon:"edit" ~items () in
  let up    = new Radio.t ~name:"dir" ~value:`Up () in
  let down  = new Radio.t ~name:"dir" ~value:`Down () in
  let left  = new Radio.t ~name:"dir" ~value:`Left () in
  let right = new Radio.t ~name:"dir" ~value:`Right () in
  let f x   = React.S.map ~eq:(fun _ _ -> false)
                          (function true -> print_endline "changed";
                                            fab#set_direction x#value | false -> ()) x#s_state in
  let _     = f up in
  let _     = f down in
  let _     = f left in
  let _     = f right in
  let dbox = new Hbox.t
                 ~widgets:[ new Form_field.t ~label:"Up" ~input:up ()
                          ; new Form_field.t ~label:"Down" ~input:down ()
                          ; new Form_field.t ~label:"Left" ~input:left ()
                          ; new Form_field.t ~label:"Right" ~input:right ()
                          ]
                 ()
  in
  let fling = new Radio.t ~name:"anim" ~value:`Fling () in
  let scale = new Radio.t ~name:"anim" ~value:`Scale () in
  let f x   = React.S.map ~eq:(fun _ _ -> false)
                          (function true -> print_endline "changed";
                                            fab#set_animation x#value | false -> ()) x#s_state in
  let _ = f fling in
  let _ = f scale in
  let abox = new Hbox.t
                 ~widgets:[ new Form_field.t ~label:"Fling" ~input:fling ()
                          ; new Form_field.t ~label:"Scale" ~input:scale ()
                          ]
                 ()
  in
  let _ = fling#set_checked true in
  let _ = up#set_checked true in
  let _ = React.E.map (fun _ -> match React.S.value fab#s_state with
                                | false -> fab#show ()
                                | true  -> fab#hide ()) fab#main#e_click
  in
  demo_section "FAB speed dial" [ dbox#widget; abox#widget; fab#widget ]

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
  React.E.map (fun _ -> checkbox#set_indeterminate @@ not checkbox#indeterminate;
                        css_checkbox#set_indeterminate @@ not css_checkbox#indeterminate)
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
  (* let title    = new Card.Title.t ~large:true ~title:"Demo card title" () in
   * let subtitle = new Card.Subtitle.t ~subtitle:"Subtitle" () in
   * let primary  = new Card.Primary.t ~widgets:[ title#widget; subtitle#widget ] () in
   * let text     = new Card.Supporting_text.t ~text:"Supporting text" () in *)
  let actions  = new Card.Actions.t ~widgets:[ new Button.t ~compact:true ~label:"action 1" ()
                                             ; new Button.t ~compact:true ~label:"action 2" () ] () in
  let card = new Card.t ~widgets:[ media#widget; actions#widget ] () in
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
  let section = demo_section "Slider" [ subsection "Continuous slider" continuous
                                      ; subsection "Discrete slider" discrete
                                      ; subsection "Discrete slider with markers" with_markers
                                      ; subsection "Disabled slider" disabled ]
  in
  let _ = React.S.map (fun x -> if x then (continuous#layout (); discrete#layout (); with_markers#layout ()))
                      section#s_expanded
  in
  section

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
  React.E.map (fun _ -> Lwt.bind (dialog#show_await ())
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
                                               ~graphic:(new Avatar.Letter.t ~text:"A" ())
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
                ~content:[ { subheader = Some (new Typography.Text.t ~text:"Group 1" ()); list = list1 }
                         ; { subheader = Some (new Typography.Text.t ~text:"Group 2" ()); list = list2 }
                ]
                () in
  group#style##.maxWidth := Js.string "400px";
  demo_section "List" [ list#widget; group#widget ]

let tree_demo () =
  let item x = new Tree.Item.t
                 ~text:("Item " ^ string_of_int x)
                 ~nested:(new Tree.t
                            ~items:[ new Tree.Item.t ~text:"Item 0"
                                       ~nested:(new Tree.t
                                                  ~items:[ new Tree.Item.t ~text:"Item 0" ()
                                                         ; new Tree.Item.t ~text:"Item 1" ()
                                                         ; new Tree.Item.t ~text:"Item 2" () ]
                                                  ()) ()
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
  let items    = List.map (fun x -> if x <> 2
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
    (Dom_html.handler (fun _ -> icon_menu#show (); Js._false))
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
  let ind_btn    = new Button.t ~label:"indeterminate" () in
  let det_btn    = new Button.t ~label:"determinate" () in
  let open_btn   = new Button.t ~label:"open" () in
  let close_btn  = new Button.t ~label:"close" () in
  let pgrs_txt   = new Typography.Text.t ~text:"Progress" () in
  let pgrs       = new Slider.t ~markers:true ~min:0.0 ~max:100.0 ~step:1. () in
  let buffer_txt = new Typography.Text.t ~text:"Buffer" () in
  let buffer     = new Slider.t ~markers:true ~min:0.0 ~max:100.0 ~step:1. () in
  let _ = React.S.map (fun x -> linear_progress#set_progress (x /. 100.)) pgrs#s_input in
  let _ = React.S.map (fun x -> linear_progress#set_buffer (x /. 100.)) buffer#s_input in
  React.E.map (fun _ -> linear_progress#set_indeterminate true) ind_btn#e_click  |> ignore;
  React.E.map (fun _ -> linear_progress#set_indeterminate false;
                        linear_progress#set_progress (React.S.value pgrs#s_value /. 100.);
                        linear_progress#set_buffer (React.S.value buffer#s_value /. 100.))
              det_btn#e_click |> ignore;
  React.E.map (fun _ -> linear_progress#show) open_btn#e_click                   |> ignore;
  React.E.map (fun _ -> linear_progress#hide) close_btn#e_click                  |> ignore;
  let btn_box = new Vbox.t ~widgets:[ind_btn; det_btn; open_btn; close_btn ] () in
  let sld_box = new Vbox.t ~widgets:[pgrs_txt#widget; pgrs#widget; buffer_txt#widget; buffer#widget] () in
  btn_box#set_justify_content `Start;
  btn_box#set_align_items `Start;
  btn_box#set_gap 20;
  linear_progress#style##.marginTop := Js.string "50px";
  sld_box#style##.marginTop := Js.string "50px";
  let sect = demo_section "Linear progress" [ btn_box#widget; sld_box#widget; linear_progress#widget ] in
  let _ = React.S.map (fun _ -> pgrs#layout (); buffer#layout ()) sect#s_expanded in
  sect

let tabs_demo () =
  let open Components.Tabs in
  let idx       = new Textfield.t ~input_id:"idx" ~input_type:(Integer (None,None)) ~label:"index" () in
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
  let section = demo_section "Tabs" [ (subsection "With icon labels" icon_bar)#widget
                                    ; (subsection "With text labels" text_bar)#widget
                                    ; idx#widget
                                    ; add#widget
                                    ; remove#widget
                                    ; (subsection "With icon and text labels" both_bar)#widget
                                    ; (subsection "With scroller" scrl_bar)#widget
                                    ]
  in
  let _ = React.S.map (fun x -> if x then (icon_bar#layout (); text_bar#layout ();
                                           both_bar#layout (); scrl_bar#layout ()))
                      section#s_expanded
  in
  section

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
                   ~input_id:"js"
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
                   ~input_id:"dense"
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
                     ~input_id:"lead_icon"
                     ~input_type:Widget.Text
                     ~label:"textfield label"
                     ~icon:{ icon      = "event"
                           ; clickable = false
                           ; pos       = `Leading
                     }
                     () in
  let trail_icon = new Textfield.t
                     ~input_id:"trail_icon"
                     ~input_type:Widget.Text
                     ~label:"textfield label"
                     ~icon:{ icon      = "delete"
                           ; clickable = false
                           ; pos       = `Trailing
                     }
                     () in
  let outlined = new Textfield.t
                   ~input_id:"outlined"
                   ~input_type:Widget.Text
                   ~label:"textfield label"
                   ~icon:{ icon   = "settings"
                         ; clickable = false
                         ; pos   = `Trailing
                   }
                   ~outline:true
                   () in
  (* Textareas *)
  let css_textarea      = new Textarea.Pure.t
                            ~input_id:"css_textarea"
                            ~placeholder:"Enter something"
                            ~rows:8 ~cols:40 () in
  let textarea          = new Textarea.t
                              ~input_id:"textarea"
                              ~label:"textarea label"
                              ~rows:8 ~cols:40 () in
  demo_section "Textfield" [ subsection "CSS only textfield" css_form
                           ; subsection "JS textfield" js
                           ; subsection "Dense textfield (with email validation)" dense
                           ; subsection "With leading icon" lead_icon
                           ; subsection "With trailing icon" trail_icon
                           ; subsection "Outlined" outlined
                           ; subsection "Textarea (css only)" css_textarea
                           ; subsection "Textarea" textarea ]

let select_demo () =
  let select = new Select.t
                   ~label:"Demo select"
                   ~items:[ `Group (new Select.Group.t
                                        ~label:"Group 1"
                                        ~items:[ new Select.Item.t ~value:() ~text:"Item 1" ()
                                               ; new Select.Item.t ~value:() ~text:"Item 2" ()
                                               ; new Select.Item.t ~value:() ~text:"Item 3" () ]
                                        ())
                          ; `Item (new Select.Item.t ~value:() ~text:"Item 1" ())
                          ; `Item (new Select.Item.t ~value:() ~text:"Item 2" ())
                          ; `Item (new Select.Item.t ~value:() ~text:"Item 3" ())
                          ]
                   ()
  in
  let disabled = new Select.t ~label:"Disabled"
                     ~disabled:true
                     ~items:[ `Item (new Select.Item.t ~value:() ~text:"Item 1" ()) ]
                     ()
  in
  let () = select#style##.width := Js.string "200px" in
  let () = disabled#style##.width := Js.string "200px" in
  demo_section "Select" [ subsection "Select" select; subsection "Disabled" disabled ]

let elevation_demo () =
  let d       = Widget.create (Html.div ~a:[Html.a_style "height: 200px; width: 200px; margin: 20px"] []
                               |> To_dom.of_element) in
  let slider  = new Slider.t ~markers:true ~max:24.0 () in
  let _       = React.S.map (fun v -> Elevation.set_elevation d @@ int_of_float v) slider#s_input in
  let section = demo_section "Elevation" [ d#widget; slider#widget ] in
  let _       = React.S.map (fun x -> if x then slider#layout ()) section#s_expanded in
  section

let table_demo () =
  let tz = let d = Js.Unsafe.new_obj (Js.Unsafe.global##.Date) [||] in
           ((Js.Unsafe.meth_call d "getTimezoneOffset" [||] : int) * (-60))
  in
  let show_time = Format.asprintf "%a" (Common.Time.pp_human ~tz_offset_s:tz ()) in
  let time  = Table.({ to_string = show_time; compare = String.compare; is_numeric = false }) in
  let fmt   = Table.((   to_column ~sortable:true "Date",     Custom time)
                     :: (to_column ~sortable:true "Input",    String)
                     :: (to_column ~sortable:true "Service",  String)
                     :: (to_column ~sortable:true "PID",      Int)
                     :: (to_column ~sortable:true "Severity", Option (String,""))
                     :: (to_column ~sortable:true "Check",    String)
                     :: (to_column "Message",                 String)
                     :: []) in
  let table = new Table.t ~selection:`Multiple ~fmt () in
  let channels = [| "BBC"; "CNN"; "MTV"; "AnimalPlanet" |] in
  let err      = [| "1.3.1 PAT error"; "1.4. Continuity count error" |] in
  let make_row () =
    let pid = Random.run (Random.int 8192) in
    let ch  = channels.(Random.run (Random.int 4)) in
    let inp = Ipaddr.V4.make 224 1 2 (Random.run (Random.int 4)) in
    let err = err.(Random.run (Random.int 2)) in
    table#add_row (Common.Time.Clock.now ())
      (Ipaddr.V4.to_string inp) ch pid (Some "Warning") err
      "Error description here"
  in
  List.iter (fun _ -> make_row ()) @@ List.range' 0 7;
  demo_section "Table" [ table#widget ]

let chart_demo () =
  let range = 10 in
  let x = ref 40 in
  Random.init (Unix.time () |> int_of_float);
  let open Chartjs.Line in
  let to_data () = List.map (fun x -> { x ; y = Random.run (Random.int range) }) (List.range_by ~step:2 0 !x) in
  let x_axis   = new Chartjs.Line.Axes.Linear.t ~id:"x" ~position:`Bottom ~typ:Int ~delta:!x () in
  let y_axis   = new Chartjs.Line.Axes.Linear.t ~id:"y" ~position:`Top ~typ:Int () in
  let options  = new Chartjs.Line.Options.t ~x_axes:[x_axis] ~y_axes:[y_axis] () in
  let datasets = List.map (fun x -> new Chartjs.Line.Dataset.t ~x_axis ~y_axis ~label:x ~data:(to_data ()) ())
                          ["Dataset 1"; "Dataset 2"]
  in
  options#hover#set_mode `Index;
  options#hover#set_axis `X;
  options#hover#set_intersect true;
  options#tooltip#set_mode `Index;
  options#tooltip#set_intersect false;
  options#elements#line#set_border_width 3;
  y_axis#ticks#set_suggested_max range;
  x_axis#scale_label#set_label_string "x axis";
  x_axis#scale_label#set_display true;
  List.iter (fun x -> if String.equal x#label "Dataset 1"
                      then x#set_border_color @@ Color.rgb_of_name (Color.Lime C500)
                      else x#set_border_color @@ Color.rgb_of_name (Color.Pink C500);
                      x#set_cubic_interpolation_mode `Monotone;
                      x#set_fill `Disabled) datasets;
  let update = new Button.t ~label:"update" () in
  let push   = new Button.t ~label:"push" () in
  let push_less = new Button.t ~label:"push less" () in
  let append    = new Button.t ~label:"append" () in
  let chart  = new Chartjs.Line.t ~options ~datasets () in
  React.E.map (fun _ -> List.iter (fun x -> x#set_point_radius (`Fun (fun _ x -> if x.x mod 2 > 0 then 10 else 5))
                                  ) datasets;
                        chart#update None)
              update#e_click |> ignore;
  React.E.map (fun _ -> x := !x + 2;
                        List.iter (fun ds -> ds#push { x = !x; y = Random.run (Random.int range) }) datasets;
                        chart#update None)
              push#e_click |> ignore;
  React.E.map (fun _ -> List.iter (fun ds -> ds#push { x = !x - 1; y = Random.run (Random.int range) }) datasets;
                        chart#update None)
              push_less#e_click |> ignore;
  React.E.map (fun _ -> x := !x + 6;
                        List.iter (fun ds -> ds#append [ { x = !x - 6; y = Random.run (Random.int range) }
                                                       ; { x = !x - 4; y = Random.run (Random.int range) }
                                                       ; { x = !x - 2; y = Random.run (Random.int range) }
                                                       ; { x = !x    ; y = Random.run (Random.int range) } ])
                                  datasets;
                        chart#update None)
              append#e_click |> ignore;
  let w = Html.div ~a:[ Html.a_style "max-width:700px"] [ Widget.to_markup chart
                                                        ; Widget.to_markup update
                                                        ; Widget.to_markup push
                                                        ; Widget.to_markup push_less
                                                        ; Widget.to_markup append ]
          |> To_dom.of_element
          |> Widget.create
  in
  demo_section "Chart" [w]

let time_chart_demo () =
  let range_i = 20 in
  let range_f = 40. in
  Random.init (Unix.time () |> int_of_float);
  let open Chartjs.Line in
  let delta    = Common.Time.Span.of_int_s 40 in
  let x_axis   = new Chartjs.Line.Axes.Time.t ~id:"x" ~position:`Bottom ~typ:Ptime ~delta () in
  let y_axis   = new Chartjs.Line.Axes.Linear.t ~id:"y" ~position:`Left ~typ:Int () in
  let y2_axis  = new Chartjs.Line.Axes.Logarithmic.t ~id:"y2" ~position:`Right ~typ:Float () in
  let options  = new Chartjs.Line.Options.t
                   ~x_axes:[x_axis]
                   ~y_axes:[ y_axis#coerce_base
                           ; y2_axis#coerce_base
                   ]
                   ()
  in
  let dataset1 = new Chartjs.Line.Dataset.t ~x_axis ~y_axis ~label:"Dataset 1" ~data:[] () in
  let dataset2 = new Chartjs.Line.Dataset.t ~x_axis ~y_axis:y2_axis ~label:"Dataset 2" ~data:[] () in
  let datasets = [ dataset1#coerce; dataset2#coerce ] in
  options#hover#set_mode `Index;
  options#hover#set_axis `X;
  options#hover#set_intersect true;
  options#tooltip#set_mode `Index;
  options#tooltip#set_intersect false;
  options#elements#line#set_border_width 3;
  x_axis#scale_label#set_label_string "x axis";
  x_axis#scale_label#set_display true;
  x_axis#time#set_min_unit `Second;
  x_axis#time#set_tooltip_format "ll HH:mm:ss";
  List.iter (fun x -> if String.equal x#label "Dataset 1"
                      then x#set_bg_color @@ Color.rgb_of_name (Color.Indigo C500)
                      else x#set_bg_color @@ Color.rgb_of_name (Color.Amber C500);
                      if String.equal x#label "Dataset 1"
                      then x#set_border_color @@ Color.rgb_of_name (Color.Indigo C500)
                      else x#set_border_color @@ Color.rgb_of_name (Color.Amber C500);
                      x#set_cubic_interpolation_mode `Monotone;
                      x#set_fill `Disabled) datasets;
  let chart = new Chartjs.Line.t ~options ~datasets () in
  let e_update,e_update_push = React.E.create () in
  React.E.map (fun () -> dataset1#push { x = Common.Time.of_float_s
                                             @@ Unix.gettimeofday () |> Option.get_exn
                                       ; y = Random.run (Random.int range_i) };
                         chart#update None)
              e_update |> ignore;
  React.E.map (fun () -> dataset2#push { x = Common.Time.of_float_s
                                             @@ Unix.gettimeofday () |> Option.get_exn
                                       ; y = Random.run (Random.float range_f) };
                         chart#update None)
              e_update |> ignore;
  Dom_html.window##setInterval (Js.wrap_callback (fun () -> e_update_push () |> ignore)) 1000. |> ignore;
  let w = Html.div ~a:[ Html.a_style "max-width:700px"] [ Widget.to_markup chart ]
          |> To_dom.of_element
          |> Widget.create
  in
  demo_section "Chart (timeline)" [w]

let dynamic_grid_demo () =
  let (props:Dynamic_grid.grid) = Dynamic_grid.to_grid ~rows:20 ~cols:30 ~min_col_width:1
                                                       ~vertical_compact:true ~items_margin:(10,10) ()
  in
  let items    = [ Dynamic_grid.Item.to_item
                     ~pos:{ x = 0
                          ; y = 0
                          ; w = 4
                          ; h = 8 }
                     ~value:()
                     ()
                 ; Dynamic_grid.Item.to_item
                     ~pos:{ x = 10
                          ; y = 0
                          ; w = 3
                          ; h = 6 }
                     ~value:()
                     ()
                 ]
  in
  let x        = new Textfield.t ~input_id:"x_field" ~label:"x position" ~input_type:(Widget.Integer (None, None)) () in
  let y        = new Textfield.t ~input_id:"y_field" ~label:"y position" ~input_type:(Widget.Integer (None, None)) () in
  let w        = new Textfield.t ~input_id:"w_field" ~label:"width"      ~input_type:(Widget.Integer (None, None)) () in
  let h        = new Textfield.t ~input_id:"h_field" ~label:"height"     ~input_type:(Widget.Integer (None, None)) () in
  let add      = new Button.t ~label:"add" () in
  let rem_all  = new Button.t ~label:"remove all" () in
  let grid     = new Dynamic_grid.t ~grid:props ~items () in
  React.E.map (fun _ -> grid#remove_all) rem_all#e_click
  |> ignore;
  React.E.map (fun _ -> match React.S.value x#s_input,React.S.value y#s_input,
                              React.S.value w#s_input,React.S.value h#s_input with
                        | Some x, Some y, Some w, Some h ->
                           grid#add (Dynamic_grid.Item.to_item
                                       ~pos:{ x; y; w; h }
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
                                         ; rem_all#widget
                                         ]
  in
  let _ = React.S.map (fun x -> if x then grid#layout ()) sect#s_expanded in
  sect

let expansion_panel_demo () =
  let ep1 = new Expansion_panel.t
              ~title:"Trip name"
              ~details:[ new Vbox.t ~widgets:[ new Typography.Text.t ~text:"Caribbean cruise" ()
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
  ep1#add_class (Elevation.Markup.get_elevation_class 2);
  ep2#add_class (Elevation.Markup.get_elevation_class 2);
  ep3#add_class (Elevation.Markup.get_elevation_class 2);
  let box = new Vbox.t ~widgets:[ep1;ep2;ep3] () in
  demo_section "Expansion panel" [ box ]

let hexdump_demo () =
  let el = new Hexdump.t ~config:(Hexdump.to_config ~base:`Hex ~grouping:1 ~width:16 ())
             "@ð®5íð@DTT - Russian Federation" () in
  demo_section "Hexdump" [ el ]

let split_demo () =
  let p s =
    let w = new Typography.Text.t ~text:s () in
    w#style##.margin := Js.string "20px";
    w in
  let el = new Hsplit.t (p "test1") (p "test2") () in
  el#panel_1#style##.border  := Js.string "1px solid lightgrey";
  el#panel_2#style##.border  := Js.string "1px solid lightgrey";
  el#style##.height := Js.string "300px";
  demo_section ~expanded:true "#Split" [ el ]


let add_demos demos =
  let demos = CCList.sort (fun x y -> CCString.compare x#title y#title) demos in
  Html.div ~a:[ Html.a_id "demo-div" ]
  @@ CCList.map (fun x -> Of_dom.of_element x#root) demos
  |> To_dom.of_element

let onload _ =
  let demos = add_demos [ expansion_panel_demo ()
                        ; dynamic_grid_demo ()
                        ; table_demo ()
                        ; button_demo ()
                        ; chart_demo ()
                        ; time_chart_demo ()
                        ; fab_demo ()
                        ; fab_speed_dial_demo ()
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
                        (* ; menu_demo () *)
                        ; snackbar_demo ()
                        ; linear_progress_demo ()
                        ; circular_progress_demo ()
                        ; tabs_demo ()
                        ; hexdump_demo ()
                        ; split_demo ()
                        ] in
  let _ = new Page.t (`Static [Widget.create demos]) () in
  Js._false

let () = Dom_html.addEventListener Dom_html.document
           Dom_events.Typ.domContentLoaded
           (Dom_html.handler onload)
           Js._false
         |> ignore
