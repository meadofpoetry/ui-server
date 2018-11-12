open Containers
open Components
open Tyxml_js
open Common

let demo_section ?expanded title content =
  new Expansion_panel.t ?expanded ~title ~content ()

let subsection name w =
  Html.div [ Html.h3 ~a:[Html.a_class [Typography.font_to_class Subtitle_2]]
               [Html.pcdata name]
           ; Widget.to_markup w ]
  |> To_dom.of_element
  |> Widget.create

let button_demo () =
  let raised = new Button.t ~label:"raised" ~style:`Raised () in
  let flat = new Button.t ~label:"flat" () in
  let unelevated = new Button.t ~label:"unelevated" ~style:`Unelevated () in
  let stroked = new Button.t ~label:"stroked" ~style:`Stroked () in
  let ripple = new Button.t ~label:"ripple" ~ripple:true () in
  let dense = new Button.t ~label:"dense" ~dense:true () in
  let compact = new Button.t ~label:"compact" ~compact:true () in
  let icon = Icon.SVG.(create_simple Path.heart) in
  let icon = new Button.t ~label:"icon" ~icon () in
  let box =
    new Vbox.t
      ~widgets:[ raised
               ; flat
               ; unelevated
               ; stroked
               ; ripple
               ; dense
               ; compact
               ; icon ] () in
  box#set_align_items `Start;
  demo_section "Button" [box]

let circular_progress_demo () =
  let open Circular_progress in
  let indeterminate = new t ~indeterminate:true () in
  let determinate = new t ~indeterminate:false ~value:45. ~max:100. () in
  let section =
    demo_section "Circular progress"
      [ subsection "Indeterminate" indeterminate
      ; subsection "Determinate" determinate
      ] in
  section

let fab_demo () =
  let icon () = Icon.SVG.(create_simple Path.heart) in
  let fab = new Fab.t ~icon:(icon ()) () in
  let mini = new Fab.t ~mini:true ~icon:(icon ()) () in
  let ripple = new Fab.t ~ripple:true ~icon:(icon ()) () in
  let box = new Vbox.t ~widgets:[ subsection "General" fab
                                ; subsection "Mini" mini
                                ; subsection "Ripple" ripple ] ()
  in
  demo_section "FAB" [box]

let fab_speed_dial_demo () =
  let items =
    List.map (fun path ->
        let icon = Icon.SVG.create_simple path in
        new Fab.t ~icon ()) Icon.SVG.Path.[face; plus; close] in
  let icon = Icon.SVG.create_simple Icon.SVG.Path.pencil in
  let fab = new Fab_speed_dial.t ~icon ~items () in
  let up = new Radio.t ~name:"dir" ~value:`Up () in
  let down = new Radio.t ~name:"dir" ~value:`Down () in
  let left = new Radio.t ~name:"dir" ~value:`Left () in
  let right = new Radio.t ~name:"dir" ~value:`Right () in
  let f x =
    React.S.map ~eq:Equal.unit
      (function true ->
                 fab#set_direction x#value | false -> ()) x#s_state
    |> React.S.keep in
  f up;
  f down;
  f left;
  f right;
  let dbox =
    new Hbox.t
      ~widgets:[ new Form_field.t ~label:"Up" ~input:up ()
               ; new Form_field.t ~label:"Down" ~input:down ()
               ; new Form_field.t ~label:"Left" ~input:left ()
               ; new Form_field.t ~label:"Right" ~input:right () ] () in
  let fling = new Radio.t ~name:"anim" ~value:`Fling () in
  let scale = new Radio.t ~name:"anim" ~value:`Scale () in
  let f x =
    React.S.map ~eq:Equal.unit
      (function true ->
                 print_endline "changed";
                 fab#set_animation x#value | false -> ()) x#s_state
    |> React.S.keep in
  f fling;
  f scale;
  let abox =
    new Hbox.t
      ~widgets:[ new Form_field.t ~label:"Fling" ~input:fling ()
               ; new Form_field.t ~label:"Scale" ~input:scale () ] () in
  fling#set_checked true;
  up#set_checked true;
  fab#main#listen_click_lwt (fun _ _ ->
      begin match React.S.value fab#s_state with
      | false -> fab#show ()
      | true  -> fab#hide ()
      end |> Lwt.return) |> Lwt.ignore_result;
  demo_section "FAB speed dial"
    [ dbox#widget; abox#widget; fab#widget ]

let radio_demo () =
  let radio1 = new Radio.t ~name:"radio" ~value:() () in
  let radio2 = new Radio.t ~name:"radio" ~value:() () in
  let radio3 = new Radio.t ~name:"radio" ~value:() () in
  demo_section "Radio button" [ radio1; radio2; radio3 ]

let checkbox_demo () =
  let checkbox = new Checkbox.t ~input_id:"checkbox-demo" () in
  let css_checkbox = new Checkbox.t ~ripple:false () in
  let btn = new Button.t ~label:"toggle indeterminate" () in
  let form_field =
    new Form_field.t
      ~label:"checkbox label"
      ~input:checkbox () in
  btn#listen_click_lwt (fun _ _ ->
    checkbox#set_indeterminate @@ not checkbox#indeterminate;
    css_checkbox#set_indeterminate @@ not css_checkbox#indeterminate;
    Lwt.return_unit) |> Lwt.ignore_result;
  demo_section "Checkbox"
    [ (subsection "Checkbox (css only)" css_checkbox)#widget
    ; (subsection "Checkbox with label" form_field)#widget
    ; btn#widget ]

let switch_demo () =
  let switch = new Switch.t ~input_id:"demo-switch" () in
  let form = new Form_field.t ~label:"switch label" ~input:switch () in
  demo_section "Switch"
    [ subsection "Switch" @@ new Switch.t ()
    ; subsection "Switch with label" form ]

let toggle_demo () =
  let on = Icon.SVG.(create_simple Path.heart) in
  let off = Icon.SVG.(create_simple Path.heart_outline) in
  let toggle =
    new Icon_button.t
      ~on_icon:on
      ~icon:off
      () in
  demo_section "Icon toggle" [ toggle ]

let card_demo () =
  let media = new Card.Media.t ~widgets:[] () in
  let url = "url(\"https://maxcdn.icons8.com/app/uploads/2016/03/material-1-1000x563.jpg\")" in
  media#style##.backgroundImage := Js.string url;
  (Js.Unsafe.coerce media#style)##.backgroundSize := Js.string "cover";
  media#style##.backgroundRepeat := Js.string "no-repeat";
  media#style##.height := Js.string "12.313rem";
  let actions =
    new Card.Actions.t
      ~widgets:[ new Button.t ~compact:true ~label:"action 1" ()
               ; new Button.t ~compact:true ~label:"action 2" () ] () in
  let card = new Card.t ~widgets:[ media#widget; actions#widget ] () in
  card#style##.width := Js.string "320px";
  demo_section "Card" [ card ]

let grid_list_demo () =
  let tiles =
    List.map (fun x ->
        new Grid_list.Tile.t
          ~src:"https://cs5-3.4pda.to/5290239.png"
          ~title:("My tile " ^ (string_of_int x))
          ~support_text:"Some text here"
          ())
      (List.range 0 4) in
  let grid = new Grid_list.t ~tiles () in
  demo_section "Grid list" [ grid ]

let ripple_demo () =
  let bounded =
    Widget.create (
        Html.div ~a:[ Html.a_class ["demo-ripple-box"]
                    ; Html.a_tabindex 0 ] []
        |> To_dom.of_element) in
  Elevation.set_elevation bounded 5;
  let ripple = Ripple.attach_to bounded in
  ignore ripple;
  let unbounded = new Icon.Font.t ~icon:"favorite" () in
  unbounded#set_attribute "tabindex" "0";
  unbounded#add_class "demo-ripple-unbounded";
  let ripple' = Ripple.attach_to ~unbounded:true unbounded in
  ignore ripple';
  let section =
    demo_section
      "Ripple"
      [ subsection "Bounded ripple. Click me!" bounded
      ; subsection "Unbounded ripple. Click me!" unbounded] in
  section

let layout_grid_demo () =
  let cells =
    List.map (fun x ->
        let w =
          Html.div ~a:[Html.a_style "box-sizing: border-box;\
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
        new Layout_grid.Cell.t ~span:1 ~widgets:[w] ())
      (List.range 0 15) in
  let btn2 = new Button.t ~label:"set span 1" () in
  let btn4 = new Button.t ~label:"set span 2" () in
  btn2#listen_click_lwt (fun _ _ ->
      (List.get_at_idx_exn 4 cells)#set_span @@ Some 1;
      Lwt.return_unit) |> Lwt.ignore_result;
  btn4#listen_click_lwt (fun _ _ ->
      (List.get_at_idx_exn 4 cells)#set_span @@ Some 2;
      Lwt.return_unit) |> Lwt.ignore_result;
  let layout_grid = new Layout_grid.t ~cells () in
  demo_section "Layout grid" [ layout_grid#widget; btn2#widget; btn4#widget ]

let dialog_demo () =
  let accept = new Button.t ~label:"Accept" () in
  let cancel = new Button.t ~label:"Decline" () in
  let dialog =
    new Dialog.t
      ~title:"This is dialog"
      ~content:(`String "Dialog body")
      ~actions:[ Dialog.Action.make ~typ:`Cancel accept
               ; Dialog.Action.make ~typ:`Accept cancel
      ]
      () in
  let button = new Button.t ~label:"show dialog" () in
  button#listen_click_lwt (fun _ _ ->
      let open Lwt.Infix in
      dialog#show_await ()
      >|= function
      | `Accept -> print_endline "Dialog accepted"
      | `Cancel -> print_endline "Dialog cancelled")
  |> Lwt.ignore_result;
  demo_section "Dialog" [ dialog#widget; button#widget ]

let list_demo () =
  let items =
    List.map (fun x ->
        if x = 3
        then `Divider (new Divider.t ())
        else `Item (new Item_list.Item.t
                      ~text:("List item " ^ (string_of_int x))
                      ~secondary_text:"some subtext here"
                      ~graphic:(new Avatar.Letter.t ~text:"A" ())
                      ~value:()
                      ()))
      (List.range 0 5) in
  let list = new Item_list.t ~avatar:true ~items () in
  list#style##.maxWidth := Js.string "400px";
  let list1 =
    new Item_list.t
      ~items:[ `Item (new Item_list.Item.t
                        ~text:"Item 1"
                        ~secondary_text:"Subtext"
                        ~value:() ())
             ; `Item (new Item_list.Item.t
                        ~text:"Item 2"
                        ~secondary_text:"Subtext"
                        ~value:() ())
             ; `Item (new Item_list.Item.t
                        ~text:"Item 3"
                        ~secondary_text:"Subtext"
                        ~value:() ())
      ]
      () in
  let list2 =
    new Item_list.t
      ~items:[ `Item (new Item_list.Item.t
                        ~text:"Item 1"
                        ~secondary_text:"Subtext"
                        ~value:() ())
             ; `Item (new Item_list.Item.t
                        ~text:"Item 2"
                        ~secondary_text:"Subtext"
                        ~value:() ())
             ; `Item (new Item_list.Item.t
                        ~text:"Item 3"
                        ~secondary_text:"Subtext"
                        ~value:() ())
      ]
      () in
  let group =
    new Item_list.List_group.t
      ~content:[ { subheader = Some (new Typography.Text.t ~text:"Group 1" ())
                 ; list = (list1 :> Item_list.base) }
               ; { subheader = Some (new Typography.Text.t ~text:"Group 2" ())
                 ; list = (list2 :> Item_list.base) }
      ]
      () in
  group#style##.maxWidth := Js.string "400px";
  demo_section "List" [ list#widget; group#widget ]

let tree_demo () =
  let item x =
    new Tree.Item.t
      ~text:("Item " ^ string_of_int x)
      ~nested:(
        new Tree.t
          ~level:1
          ~items:[ new Tree.Item.t ~text:"Item 0" ~value:()
                     ~nested:(
                       new Tree.t
                         ~level:2
                         ~items:[ new Tree.Item.t ~text:"Item 0" ~value:() ()
                                ; new Tree.Item.t ~text:"Item 1" ~value:() ()
                                ; new Tree.Item.t ~text:"Item 2" ~value:() () ]
                         ()) ()
                 ; new Tree.Item.t ~text:"Item 1" ~value:() ()
                 ; new Tree.Item.t ~text:"Item 2" ~value:() () ]
          ())
      ~value:()
      () in
  let tree =
    new Tree.t
      ~level:0
      ~items:(List.map (fun x -> item x) (List.range 0 5))
      () in
  tree#style##.maxWidth := Js.string "400px";
  demo_section "Tree" [ tree ]

let menu_demo () =
  let items =
    List.map (fun x ->
        if x <> 2
        then `Item (new Menu.Item.t
                      ~text:("Menu item " ^ (string_of_int x))
                      ~value:() ())
        else `Divider (new Divider.t ()))
      (List.range 0 5) in
  let anchor = new Button.t ~label:"Open menu" () in
  anchor#style##.marginBottom := Js.string "50px";
  let menu = new Menu.t ~items () in
  let wrapper = new Menu.Wrapper.t ~menu ~anchor () in
  menu#set_dense true;
  let icon_anchor = new Icon.Font.t ~icon:"more_horiz" () in
  let icon_menu =
    new Menu.t
      ~items:[ `Item (new Menu.Item.t ~text:"Item 1" ~value:() ())
             ; `Item (new Menu.Item.t ~text:"Item 2" ~value:() ())
             ; `Item (new Menu.Item.t ~text:"Item 3" ~value:() ()) ]
      () in
  let icon_wrapper = new Menu.Wrapper.t ~menu:icon_menu ~anchor:icon_anchor () in
  anchor#listen_click_lwt (fun _ _ -> menu#show (); Lwt.return_unit)
  |> Lwt.ignore_result;
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

let tabs_demo () =
  let idx = new Textfield.t
              ~input_id:"idx"
              ~input_type:(Integer (None,None))
              ~label:"index" () in
  let add = new Button.t ~label:"add" () in
  let remove = new Button.t ~label:"remove" () in
  let icon_bar =
    let icon = Icon.SVG.create_simple in
    [ new Tab.t ~content:(Icon (icon Icon.SVG.Path.paw)) ~value:() ()
    ; new Tab.t ~content:(Icon (icon Icon.SVG.Path.heart)) ~value:() ()
    ; new Tab.t ~content:(Icon (icon Icon.SVG.Path.star)) ~value:() ()
    ; new Tab.t ~content:(Icon (icon Icon.SVG.Path.map_marker)) ~value:() ()
    ] |> (fun tabs -> new Tab_bar.t ~tabs ()) in
  let text_bar  =
    List.map (fun x ->
        new Tab.t ~content:(Text ("Tab " ^ (string_of_int x))) ~value:() ())
      (List.range 0 3)
    |> (fun tabs -> new Tab_bar.t ~tabs ()) in
  let both_bar  =
    let icon = Icon.SVG.create_simple in
    [ new Tab.t ~content:(Both ("Pets", icon Icon.SVG.Path.paw))
        ~value:() ()
    ; new Tab.t ~content:(Both ("Favorite", icon Icon.SVG.Path.heart))
        ~value:() ()
    ; new Tab.t ~content:(Both ("Starred", icon Icon.SVG.Path.star))
        ~value:() ()
    ; new Tab.t ~content:(Both ("GPS", icon Icon.SVG.Path.map_marker))
        ~value:() ()
    ] |> (fun tabs -> new Tab_bar.t ~tabs ()) in
  let section =
    demo_section "Tabs"
      [ (subsection "With icon labels" icon_bar)#widget
      ; (subsection "With text labels" text_bar)#widget
      ; (subsection "With icon and text labels" both_bar)#widget
      ] in
  section

let textfield_demo () =
  (* Full-featured js textbox *)
  let js_helptext =
    new Textfield.Helper_text.t
      ~validation:true
      ~content:"This field must not be empty"
      () in
  let js =
    new Textfield.t
      ~input_type:Text
      ~label:"js textfield label"
      ~helper_text:js_helptext
      () in
  let js_wrapped =
    Textfield.wrap
      ~textfield:js
      ~helper_text:js_helptext in
  js#set_required true;
  (* Dense js textbox with *)
  let dense_helptext =
    new Textfield.Helper_text.t
      ~validation:true
      ~content:"Bad input"
      () in
  let dense =
    new Textfield.t
      ~label:"dense textfield label"
      ~input_type:Email
      ~helper_text:dense_helptext
      () in
  let dense_wrapped =
    Textfield.wrap
      ~textfield:dense
      ~helper_text:dense_helptext in
  dense#set_dense true;
  (* Textboxes with icons *)
  let lead_icon =
    new Textfield.t
      ~input_type:Text
      ~label:"textfield label"
      ~leading_icon:(Icon.SVG.(create_simple Path.earth))
      () in
  let trail_icon =
    new Textfield.t
      ~input_type:Text
      ~label:"textfield label"
      ~trailing_icon:(Icon.SVG.(create_simple Path.delete))
      () in
  let outlined =
    new Textfield.t
      ~input_type:Text
      ~label:"textfield label"
      ~outlined:true
      () in
  (* Textareas *)
  let textarea =
    Textfield.make_textarea
      ~label:"textarea label"
      ()
    |> fun x ->
       let w = Widget.create_div ~widgets:[x] () in
       let box = Widget.create_div ~widgets:[w] () in
       box#add_class "mdc-text-field-row";
       w#add_class Textfield.Markup.container_class;
       box in
  demo_section "Textfield"
    [ subsection "JS textfield" js_wrapped
    ; subsection "Dense textfield (with email validation)" dense_wrapped
    ; subsection "With leading icon" lead_icon
    ; subsection "With trailing icon" trail_icon
    ; subsection "Outlined" outlined
    ; subsection "Textarea" textarea ]

let select_demo () =
  let select =
    new Select.t
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
  let disabled =
    new Select.t ~label:"Disabled"
      ~disabled:true
      ~items:[ `Item (new Select.Item.t ~value:() ~text:"Item 1" ()) ]
      ()
  in
  let () = select#style##.width := Js.string "200px" in
  let () = disabled#style##.width := Js.string "200px" in
  demo_section "Select" [ subsection "Select" select
                        ; subsection "Disabled" disabled ]

let table_demo () =
  let fmt   =
    let open Table in
    let open Format in
    Table.((   to_column ~sortable:true "Date",     Time None)
           :: (to_column ~sortable:true "Input",    String None)
           :: (to_column ~sortable:true "Service",  String None)
           :: (to_column ~sortable:true "PID",      Int None)
           :: (to_column ~sortable:true "Severity", Option (String None,""))
           :: (to_column ~sortable:true "Check",    String None)
           :: (to_column "Message",                 String None)
           :: []) in
  let table = new Table.t ~selection:`Multiple ~fmt () in
  let channels = [| "BBC"; "CNN"; "MTV"; "AnimalPlanet" |] in
  let err      = [| "1.3.1 PAT error"; "1.4. Continuity count error" |] in
  let make_row () =
    let pid = Random.run (Random.int 8192) in
    let ch  = channels.(Random.run (Random.int 4)) in
    let inp = Ipaddr.V4.make 224 1 2 (Random.run (Random.int 4)) in
    let err = err.(Random.run (Random.int 2)) in
    table#add_row
      ((Common.Time.Clock.now ())
       :: (Ipaddr.V4.to_string inp)
       :: ch
       :: pid
       :: (Some "Warning")
       :: err
       :: "Error description here"
       :: []) in
  List.iter (fun _ -> make_row () |> ignore) @@ List.range' 0 7;
  let row = table#rows |> List.hd in
  demo_section "Table" [ table#widget ]

let chart_demo () =
  let open Chartjs.Line in
  let range = 10 in
  let x = ref 40 in
  Random.init (Unix.time () |> int_of_float);
  let to_data () =
    List.map (fun x -> { x; y = Random.run (Random.int range) })
      (List.range_by ~step:2 0 !x) in
  let x_axis   = new Axes.Linear.t ~id:"x" ~position:`Bottom
                   ~typ:Int ~delta:!x () in
  let y_axis   = new Axes.Linear.t ~id:"y" ~position:`Top
                   ~typ:Int () in
  let options  = new Options.t ~x_axes:[x_axis] ~y_axes:[y_axis] () in
  let datasets =
    List.map (fun x -> new Dataset.t ~x_axis ~y_axis
                         ~label:x ~data:(to_data ()) ())
      ["Dataset 1"; "Dataset 2"] in
  options#hover#set_mode `Index;
  options#hover#set_axis `X;
  options#hover#set_intersect true;
  options#tooltip#set_mode `Index;
  options#tooltip#set_intersect false;
  options#elements#line#set_border_width 3;
  y_axis#ticks#set_suggested_max range;
  x_axis#scale_label#set_label_string "x axis";
  x_axis#scale_label#set_display true;
  List.iter (fun x ->
      if String.equal x#label "Dataset 1"
      then x#set_border_color @@ Color.(RGB (rgb_of_material (Lime C500)))
      else x#set_border_color @@ Color.(RGB (rgb_of_material (Pink C500)));
      x#set_fill `Disabled) datasets;
  let update = new Button.t ~label:"update" () in
  let push = new Button.t ~label:"push" () in
  let push_less = new Button.t ~label:"push less" () in
  let append = new Button.t ~label:"append" () in
  let chart = new Chartjs.Line.t ~options ~datasets () in
  update#listen_click_lwt (fun _ _ ->
      List.iter (fun x ->
          x#set_point_radius (`Fun (fun _ x -> if x.x mod 2 > 0
                                               then 10 else 5))) datasets;
      chart#update None;
      Lwt.return_unit) |> Lwt.ignore_result;
  push#listen_click_lwt (fun _ _ ->
      x := !x + 2;
      List.iter (fun ds ->
          ds#push { x = !x; y = Random.(run (int range)) }) datasets;
      chart#update None;
      Lwt.return_unit) |> Lwt.ignore_result;
  push_less#listen_click_lwt (fun _ _ ->
      List.iter (fun ds ->
          ds#push { x = !x - 1; y = Random.(run (int range)) }) datasets;
      chart#update None;
      Lwt.return_unit) |> Lwt.ignore_result;
  append#listen_click_lwt (fun _ _ ->
      x := !x + 6;
      List.iter (fun ds ->
          ds#append [ { x = !x - 6; y = Random.run (Random.int range) }
                    ; { x = !x - 4; y = Random.run (Random.int range) }
                    ; { x = !x - 2; y = Random.run (Random.int range) }
                    ; { x = !x    ; y = Random.run (Random.int range) } ])
        datasets;
      chart#update None;
      Lwt.return_unit) |> Lwt.ignore_result;
  let w = Html.div ~a:[ Html.a_style "max-width:700px"]
            [ Widget.to_markup chart
            ; Widget.to_markup update
            ; Widget.to_markup push
            ; Widget.to_markup push_less
            ; Widget.to_markup append ]
          |> To_dom.of_element
          |> Widget.create
  in
  demo_section "Chart (Line)" [w]

let new_chart_demo () =
  let open Chartjs in
  let init =
    Line'.Dataset.[ { x = 0.; y = 10. }
                  ; { x = 5.; y = 7. }
                  ; { x = 10.; y = 12. } ] in
  let dataset =
    Line'.Dataset.make ~label:"My dataset"
      ~border_color:(CSS.Color.(string_of_name Blue))
      ~background_color:(CSS.Color.(string_of_name Red))
      ~point_radius:(`Int 7)
      ~data:init
      () in
  let data = Line'.Data.make ~datasets:[dataset] in
  let line =
    Options'.Elements.Line.make
      ~fill:(`Bool false)
      ~border_dash:[5; 10]
      () in
  let elements =
    Options'.Elements.make
      ~line
      () in
  let legend =
    let labels =
      Options'.Legend.Labels.make
        () in
    Options'.Legend.make
      ~labels
      () in
  let title =
    Chartjs.Options'.Title.make
      ~display:true
      ~text:"This is a title"
      ~position:`Right
      () in
  (* let on_resize = fun _ (size : Chartjs.Options'.size) ->
   *   Printf.printf "width: %d, height: %d\n" size.width size.height in *)
  let options =
    Chartjs.Options'.make
      ~elements
      ~legend
      ~title
      (* ~on_resize *)
      () in
  let conf = Line'.Config.make ~options ~data "line" in
  let canvas = Dom_html.(createCanvas document) in
  ignore @@ Js.Unsafe.global##.console##log conf;
  let chart = Chartjs.Line.Chart.make (`Canvas canvas) conf in
  demo_section "Chart (New)" [Widget.create canvas]

let time_chart_demo () =
  let range_i = 20 in
  let range_f = 40. in
  Random.init (Unix.time () |> int_of_float);
  let open Chartjs in
  let delta = Time.Span.of_int_s 40 in
  let x_axis = new Line.Axes.Time.t ~id:"x" ~position:`Bottom ~typ:Ptime ~delta () in
  let y_axis = new Line.Axes.Linear.t ~id:"y" ~position:`Left ~typ:Int () in
  let y2_axis = new Line.Axes.Logarithmic.t ~id:"y2" ~position:`Right ~typ:Float () in
  let options  =
    new Line.Options.t
      ~x_axes:[x_axis]
      ~y_axes:[ y_axis#coerce_base
              ; y2_axis#coerce_base ]
      () in
  let dataset1 = new Line.Dataset.t ~x_axis ~y_axis ~label:"Dataset 1" ~data:[] () in
  let dataset2 = new Line.Dataset.t ~x_axis ~y_axis:y2_axis ~label:"Dataset 2" ~data:[] () in
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
  List.iter (fun x ->
      if String.equal x#label "Dataset 1"
      then x#set_bg_color @@ Color.(of_material (Indigo C500))
      else x#set_bg_color @@ Color.(of_material (Amber C500));
      if String.equal x#label "Dataset 1"
      then x#set_border_color @@ Color.(of_material (Indigo C500))
      else x#set_border_color @@ Color.(of_material (Amber C500));
      x#set_fill `Disabled) datasets;
  let chart = new Line.t ~options ~datasets () in
  let e_update, e_update_push = React.E.create () in
  React.E.map (fun () ->
      dataset1#push { x = Time.of_float_s
                          @@ Unix.gettimeofday () |> Option.get_exn
                    ; y = Random.run (Random.int range_i) };
      chart#update None)
    e_update
  |> React.E.keep;
  React.E.map (fun () ->
      dataset2#push { x = Time.of_float_s
                          @@ Unix.gettimeofday () |> Option.get_exn
                    ; y = Random.run (Random.float range_f) };
      chart#update None)
    e_update
  |> React.E.keep;
  Dom_html.window##setInterval (Js.wrap_callback (fun () -> e_update_push () |> ignore)) 1000. |> ignore;
  let w = Html.div ~a:[ Html.a_style "max-width:700px"] [ Widget.to_markup chart ]
          |> To_dom.of_element
          |> Widget.create
  in
  demo_section "Chart (Timeline)" [w]

let dynamic_grid_demo () =
  let (props:Dynamic_grid.grid) =
    Dynamic_grid.to_grid ~rows:20 ~cols:30 ~min_col_width:1
      ~vertical_compact:true ~items_margin:(10,10) ()
  in
  let items =
    [ Dynamic_grid.Item.to_item
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
  let x = new Textfield.t
            ~input_id:"x_field"
            ~label:"x position"
            ~input_type:(Integer (None, None))
            () in
  let y = new Textfield.t
            ~input_id:"y_field"
            ~label:"y position"
            ~input_type:(Integer (None, None))
            () in
  let w = new Textfield.t
            ~input_id:"w_field"
            ~label:"width"
            ~input_type:(Integer (None, None))
            () in
  let h = new Textfield.t
            ~input_id:"h_field"
            ~label:"height"
            ~input_type:(Integer (None, None))
            () in
  let add  = new Button.t ~label:"add" () in
  let rem_all = new Button.t ~label:"remove all" () in
  let grid = new Dynamic_grid.t ~grid:props ~items () in
  grid#style##.backgroundColor := Js.string "lightgreen";
  rem_all#listen_click_lwt (fun _ _ ->
      grid#remove_all ();
      Lwt.return_unit) |> Lwt.ignore_result;
  add#listen_click_lwt (fun _ _ ->
      match React.S.value x#s_input,
            React.S.value y#s_input,
            React.S.value w#s_input,
            React.S.value h#s_input with
      | Some x, Some y, Some w, Some h ->
         grid#add (Dynamic_grid.Item.to_item
                     ~pos:{ x; y; w; h }
                     ~value:()
                     ())
         |> (function
             | Ok _ -> print_endline "ok"
             | Error _ -> ())
         |> Lwt.return
      | _ -> Lwt.return_unit) |> Lwt.ignore_result;
  let sect =
    demo_section "Dynamic grid" [ grid#widget
                                ; x#widget
                                ; y#widget
                                ; w#widget
                                ; h#widget
                                ; add#widget
                                ; rem_all#widget ] in
  React.S.map ~eq:Equal.unit (fun x -> if x then grid#layout ()) sect#s_expanded
  |> React.S.keep;
  sect

let expansion_panel_demo () =
  let open Typography in
  let ep1 =
    new Expansion_panel.t
      ~title:"Trip name"
      ~details:[ new Vbox.t
                   ~widgets:[ new Text.t ~text:"Caribbean cruise" ()
                            ; new Text.t ~text:"Second line" () ]
                   () ]
      ~content:[]
      () in
  let ep2 =
    new Expansion_panel.t
      ~title:"Location"
      ~heading_details:[ new Text.t ~text:"Optional" () ]
      ~details:[ new Text.t ~text:"Barbados" () ]
      ~content:[ new Text.t ~text:"This is an expansion panel body text!!!" () ]
      ~actions:[ new Button.t ~label:"Cancel" ()
               ; new Button.t ~label:"Save" () ]
      () in
  let ep3 =
    new Expansion_panel.t
      ~title:"Start and end dates"
      ~details:[ new Text.t ~text:"Start date: Feb 29, 2016" ()
               ; new Text.t ~text:"End date: Not set" () ]
      ~content:[]
      () in
  ep1#add_class (Elevation.Markup.get_elevation_class 2);
  ep2#add_class (Elevation.Markup.get_elevation_class 2);
  ep3#add_class (Elevation.Markup.get_elevation_class 2);
  let box = new Vbox.t ~widgets:[ep1; ep2; ep3] () in
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
  demo_section "Split" [ el ]

let pie_demo () =
  let open Chartjs in
  let options = new Pie.Options.t () in
  let dataset =
    new Pie.Dataset.t
      ~label:"Dataset1"
      Int
      Pie.Dataset.[ to_point (Color.of_material (Blue C500)) 3
                  ; to_point (Color.of_material (Amber C500)) 7
                  ; to_point (Color.of_material (Deep_orange C500)) 1 ] in
  let pie =
    new Pie.t ~options
      ~labels:["Blue"; "Amber"; "Deep orange"]
      ~datasets:[dataset] () in
  demo_section "Chart (Pie)" [pie]

let typography_demo () =
  let open Typography in
  let lorem =
    "Lorem ipsum dolor sit amet, consectetur adipisicing elit.\
     Quos blanditiis tenetur unde suscipit, quam beatae rerum\
     inventore consectetur, neque doloribus, cupiditate numquam\
     dignissimos laborum fugiat deleniti? Eum quasi quidem quibusdam." in
  let headline_1 = new Text.t ~text:"Headline 1" ~font:Headline_1 () in
  let headline_2 = new Text.t ~text:"Headline 2" ~font:Headline_2 () in
  let headline_3 = new Text.t ~text:"Headline 3" ~font:Headline_3 () in
  let headline_4 = new Text.t ~text:"Headline 4" ~font:Headline_4 () in
  let headline_5 = new Text.t ~text:"Headline 5" ~font:Headline_5 () in
  let headline_6 = new Text.t ~text:"Headline 6" ~font:Headline_6 () in
  let subtitle_1 = new Text.t ~text:"Subtitle 1" ~font:Subtitle_1 () in
  let subtitle_2 = new Text.t ~text:"Subtitle 2" ~font:Subtitle_2 () in
  let body_1     = new Text.t ~text:("Body 1. " ^ lorem) ~font:Body_1 () in
  let body_2     = new Text.t ~text:("Body 2. " ^ lorem) ~font:Body_2 () in
  let button     = new Text.t ~text:"Button text" ~font:Button () in
  let caption    = new Text.t ~text:"Caption text" ~font:Caption () in
  let overline   = new Text.t ~text:"Overline text" ~font:Overline () in
  let box = new Vbox.t ~widgets:[ headline_1
                                ; headline_2
                                ; headline_3
                                ; headline_4
                                ; headline_5
                                ; headline_6
                                ; subtitle_1
                                ; subtitle_2
                                ; body_1
                                ; body_2
                                ; button
                                ; caption
                                ; overline ] () in
  List.iter (fun w -> w#style##.marginBottom := Js.string "20px") box#widgets;
  demo_section "Typography" [ box ]

let add_demos demos =
  let demos = CCList.sort (fun x y -> CCString.compare x#title y#title) demos in
  Html.div ~a:[ Html.a_id "demo-div" ]
  @@ CCList.map (fun x -> Of_dom.of_element x#root) demos
  |> To_dom.of_element

let onload _ =
  let demos =
    add_demos [ expansion_panel_demo ()
              ; dynamic_grid_demo ()
              ; table_demo ()
              ; button_demo ()
              ; new_chart_demo ()
              ; time_chart_demo ()
              ; fab_demo ()
              ; fab_speed_dial_demo ()
              ; radio_demo ()
              ; checkbox_demo ()
              ; switch_demo ()
              ; toggle_demo ()
              ; select_demo ()
              ; textfield_demo ()
              ; card_demo ()
              ; grid_list_demo ()
              ; ripple_demo ()
              ; layout_grid_demo ()
              ; dialog_demo ()
              ; list_demo ()
              ; tree_demo ()
              ; circular_progress_demo ()
              ; tabs_demo ()
              ; hexdump_demo ()
              ; split_demo ()
              ; pie_demo ()
              ; typography_demo ()
      ] in
  ignore @@ new Ui_templates.Page.t (`Static [Widget.create demos]) ();
  Js._false

let () =
  Dom_html.addEventListener Dom_html.document
    Dom_events.Typ.domContentLoaded
    (Dom_html.handler onload)
    Js._false
  |> ignore
