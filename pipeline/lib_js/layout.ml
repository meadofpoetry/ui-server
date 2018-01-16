open Components
open React
open Tyxml_js

let js = Js.string


let demo_section ?(style="") ?(classes=[]) title content =
  List.iter (fun x -> x#style##.margin := Js.string "10px") content;
  Html.section ~a:[ Html.a_style ("margin: 24px; padding: 24px;\
                                   border: 1px solid rgba(0, 0, 0, .12);" ^ style)
                  ; Html.a_class classes ]
               ( Html.h2 ~a:[ Html.a_class [Typography.font_to_class Headline]] [Html.pcdata title]
                 :: Widget.widgets_to_markup content)
  |> To_dom.of_element

let add_demos demos =
  Html.div ~a:[ Html.a_id "demo-div" ]
  @@ CCList.map (fun x -> Of_dom.of_element (x :> Dom_html.element Js.t)) demos
  |> To_dom.of_element

let initialize (resolution: int * int) (widgets: (string * Wm.widget) list) =
  let a,b = resolution in
  let (props:Dynamic_grid.grid) =
    { min_col_width    = 1
    ; max_col_width    = None
    ; cols             = 100
    ; rows             = Some 100
    ; row_height       = Some (b / 100)
    ; vertical_compact = false
    ; items_margin     = None
    } in
  let (items:'a Dynamic_grid.item list) = [] in
  let x    = new Textfield.t
                 ~label:"x position"
                 ~input_type:(Widget.Integer None)
                 () in
  let y    = new Textfield.t
                 ~label:"y position"
                 ~input_type:(Widget.Integer None)
                 () in
  let w    = new Textfield.t
                 ~label:"width"
                 ~input_type:(Widget.Integer None)
                 () in
  let h    = new Textfield.t
                 ~label:"height"
                 ~input_type:(Widget.Integer None)
                 () in
let add   = new Button.t ~label:"add" () in
  let add_free = new Button.t ~label:"add free" () in
  let grid  = new Dynamic_grid.t ~grid:props ~items () in
  React.E.map (fun () -> let open Lwt.Infix in
                         let min_w, min_h = 1, 1 in
                         grid#add_free ~min_w ~min_h ()
                         >>= (function
                              | Ok _ -> print_endline "ok"; Lwt.return_unit
                              | Error _ -> print_endline "error"; Lwt.return_unit)
                         |> ignore) add_free#e_click
  |> ignore;
  React.E.map (fun () -> match React.S.value x#s_input,React.S.value y#s_input,
                               React.S.value w#s_input,React.S.value h#s_input with
                         | Some x, Some y, Some w, Some h ->
                            grid#add { pos	 = { x; y; w; h }
                                     ; min_w     = Some 3
                                     ; min_h     = Some 3
                                     ; max_w     = None
                                     ; max_h     = None
                                     ; static    = false
                                     ; resizable = true
                                     ; draggable = true
                                     ; widget    = None
                                     ; value     = () }
                            |> (function
                                | Ok _ -> print_endline "ok"
                                | Error l -> Printf.printf "Collides with %d items\n" @@ CCList.length l)
                         | _ -> ()) add#e_click |> ignore;
  React.S.map (fun x -> Printf.printf "%d items in grid\n" @@ CCList.length x) grid#s_items |> ignore;
  let demo = add_demos[(demo_section "Dynamic grid"
  [ grid#widget; x#widget; y#widget; w#widget; h#widget; add#widget; add_free#widget ])] in
  Dom.appendChild Dom_html.document##.body demo;
  React.S.map (fun x ->
      let layout =
        List.map (fun (x: Dynamic_grid.Position.t) ->
            "",
            ({ position = { left = x.x
                          ; right = x.x + x.w
                          ; top = x.y
                          ; bottom = x.y + x.h}
             ; widgets = []}: Wm.container)
       ) (React.S.value grid#s_change) in
      ({ resolution = resolution
       ; widgets    = []
       ; layout     = layout}: Wm.t)) grid#s_change

