open Components
open React
open Tyxml_js

let js = Js.string


let demo_section ?(style = "") ?(classes = []) title content =
  List.iter (fun x -> x#style##.margin := Js.string "10px") content;
  Html.section ~a:[ Html.a_style ("margin: 24px; padding: 24px;\
                                   border: 1px solid rgba(0, 0, 0, .12);" ^ style)
                  ; Html.a_class classes ]
               ( Html.h2 ~a:[ Html.a_class [Typography.font_to_class Headline]]
                         [Html.pcdata title] :: Widget.widgets_to_markup content)
  |> To_dom.of_element

let add_demos demos =
  Html.div ~a:[ Html.a_id "demo-div" ]
  @@ CCList.map (fun x -> Of_dom.of_element (x :> Dom_html.element Js.t)) demos
  |> To_dom.of_element


let initialize d (resolution: int * int) (widgets1: (string * Wm.widget) list) =
  let acc = List.fold_left (fun acc (str,_) ->
                let radio = new Radio.t ~name:str ~value:str in
                List.append acc [radio]
    ) [] widgets1 in
  let dialog = new Dialog.t
                   ~title:"What widget u'd like to add?"
                   ~content:(`Widgets acc)
                   ~actions:[ new Dialog.Action.t ~typ:`Decline ~label:"Decline" ()
                            ; new Dialog.Action.t ~typ:`Accept  ~label:"Accept"  ()
                            ]
                   () in
  Dom.appendChild Dom_html.document##.body dialog#root;
  let a, b = resolution in
  let asp = float_of_int a /. float_of_int b in
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
  let add_free = new Button.t ~label:"add" () in
  let grid  = new Dynamic_grid.t ~grid:props ~items () in
  React.E.map (fun () -> let open Lwt.Infix in
                         let min_w, min_h = 3, 3 in
                         Lwt.bind dialog#show_await
                                  (function
                                   | `Accept ->
                                      grid#add_free ~min_w ~min_h ()
                                      >>= (function
                                           | Ok _    -> print_endline "ok"; Lwt.return_unit
                                           | Error _ -> print_endline "error"; Lwt.return_unit)
                                      |> ignore;
                                      Lwt.return ()
                                   | `Cancel -> print_endline "Dialog cancelled"; Lwt.return ());
                         ) add_free#e_click
  |> ignore;
  React.S.map (fun x -> Printf.printf "%d items in grid\n" @@ CCList.length x) grid#s_items
  |> ignore;
  let demo = add_demos[(demo_section "Dynamic grid" [grid#widget; add_free#widget])] in
  Dom.appendChild d demo;
  React.S.map (fun _ ->
      let layout =
        List.map (fun (x: Dynamic_grid.Position.t) ->
            let str,wd = List.hd widgets1 in
            (* let a, b = wd.aspect in *)
            (* let wid_w, wid_h = *)
            (*   if x.pos.w / a * b > x.pos.h *)
            (*   then x.pos.h / b * a, x.pos.h *)
            (*   else x.pos.w, x.pos.w / a * b in *)
            let width, height = grid#root##.offsetWidth, grid#root##.offsetHeight in
            let col_s = width / props.cols in
            let row_s = match props.rows with
              | Some x -> height / x
              | None   -> 1 in
            let real_w, real_h = resolution in
            let (container_pos : Wm.position) =
              { left   = x.x * col_s * real_w / width
              ; right  = (x.x + x.w) * col_s * real_w / width
              ; top    = (x.y * row_s) * real_h / height
              ; bottom = (x.y + x.h) * row_s * real_h / height } in
            let wd1 = {wd with position = container_pos } in
            "Vid_01011",
            ({ position = container_pos
             ; widgets  = [(str,wd1)]}: Wm.container)
          ) (React.S.value grid#s_change) in
      ({ resolution = resolution
       ; widgets    = widgets1
       ; layout     = layout}: Wm.t)) grid#s_change

