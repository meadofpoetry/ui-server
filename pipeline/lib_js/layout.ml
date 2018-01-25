open Components
open React
open Tyxml_js

let js = Js.string

(* returns true if el matches any part of the list*)
let refers_to_any (el: string * Wm.widget) (list: (string * Wm.widget) list) =
  CCList.fold_while (fun acc x ->
    if x = el then (true, `Stop) else (acc, `Continue)) false list

(* a function that *)
let resolution_to_aspect resolution =
  let w, h = resolution in
  let rec greatest_common_divisor a b =
    if a != 0 && b != 0 then
      let a, b =
      if a > b
      then a mod b, b
      else a, b mod a
      in
      greatest_common_divisor a b
    else a + b in
  let d = greatest_common_divisor w h in
  let new_res = w / d, h / d in
  new_res

let calc_rows_cols grid_asp cont_asp list =
  let num = List.length list in
  let squares =
    List.mapi (fun rows _ ->
        let cols = ceil (float_of_int num /. float_of_int rows) in
        let w = 1. in
        let h = w /. grid_asp in
        if (w /. cols *. cont_asp) *. float_of_int rows <= h
        then int_of_float cols, rows, (w /. cols *. w /. cols *. cont_asp)
        else int_of_float cols, rows,
             (h /. (float_of_int rows) *. h /. (float_of_int rows) *. cont_asp)
      ) list in
  let (cols:int), (rows:int), _ =
    List.fold_left (fun acc x ->
        let _,_,sq = x in
        let _,_,gr = acc in
        if gr > sq then acc else x)
      (0,0,0.) squares in
  cols, rows

let section ?(style = "") ?(classes = []) title content =
  List.iter (fun x -> x#style##.margin := Js.string "Unknown source10px") content;
  Html.section ~a:[ Html.a_style ("margin: 24px; padding: 24px;\
                                   border: 1px solid rgba(0, 0, 0, .12);" ^ style)
                  ; Html.a_class classes ]
               ( Html.h2 ~a:[ Html.a_class [Typography.font_to_class Headline]]
                         [Html.pcdata title] :: Widget.widgets_to_markup content)
  |> To_dom.of_element

let add demos =
  Html.div ~a:[ Html.a_id "demo-div" ]
  @@ CCList.map (fun x -> Of_dom.of_element (x :> Dom_html.element Js.t)) demos
  |> To_dom.of_element


let initialize d (wm: Wm.t) =
  let asp  = resolution_to_aspect wm.resolution in
  let cols, rows =
    match asp with
    | 4  , 3   -> 32 , 24
    | 16 , 9   -> 32 , 18  (*HD 720, 1080*)
    | 5  , 3   -> 30 , 18
    | 5  , 4   -> 30 , 24
    | 8  , 5   -> 32 , 20
    | 25 , 16  -> 25 , 16
    | 20 , 11  -> 40 , 22
    | 256, 135 -> 256, 135 (*UHD N*K*)        (*!!!*)
    | _        -> 32 , 18
  in
  let wd_list list =
    let wds =
      List.fold_left (fun acc (x: (string * Wm.widget)) ->
          let str, _ = x in
          let label = str in
          let checkbox = new Checkbox.t () in
          checkbox#set_id label;
          if (refers_to_any x list)
          then checkbox#set_checked true
          else  checkbox#set_checked false;
          (*new Radio.t ~name:"str" ~value:x () in*)
          let form_field =
            new Form_field.t ~label ~input:checkbox () in
          List.append acc [form_field]
        ) [] wm.widgets in
    let checkbox = new Checkbox.t () in
    let form_field = new Form_field.t ~label:"choose all" ~input:checkbox () in
    React.E.map (fun checked ->
        if not checked
        then List.iter (fun x -> x#get_input_widget#set_checked false) wds
        else List.iter (fun x -> x#get_input_widget#set_checked true) wds)
    @@ React.S.changes checkbox#s_state |> ignore;
    let app = [form_field] in
    List.append wds app in

  let (props:Dynamic_grid.grid) =
    { min_col_width    = 1
    ; max_col_width    = None
    ; cols
    ; rows             = Some rows
    ; row_height       = None
    ; vertical_compact = false
    ; items_margin     = None
    } in
  let (items:'a Dynamic_grid.item list) =
    List.map
      (fun (x: string * Wm.container) ->
        let _, cont      = x in
        let res_w, res_h = wm.resolution in
        let x            = cols * cont.position.left / res_w in
        let w            = cols * (cont.position.right - cont.position.left) / res_w in
        let y            = rows * cont.position.top / res_h in
        let h            = rows * (cont.position.bottom - cont.position.top) / res_h in
        let value        =
          match cont.widgets with
          | [] -> List.hd wm.widgets
          | _ -> List.hd cont.widgets
        in
        Dynamic_grid.Item.to_item
          ~pos:{ x; y; w; h }
          ~min_w:1
          ~min_h:1
          ~value ()
      ) wm.layout in

  let add_auto = new Button.t ~label:"add auto" () in
  let remove   = new Button.t ~label:"remove" () in
  let grid  = new Dynamic_grid.t ~grid:props ~items () in

  React.E.map (fun e -> let open Lwt.Infix in
                        Dom_html.stopPropagation e;
                        grid#remove_free ()
                        >>= (function
                             | Ok _    -> print_endline "ok"   ; Lwt.return_unit
                             | Error _ -> print_endline "error"; Lwt.return_unit)
                        |> ignore) remove#e_click
  |> ignore;

  React.E.map (fun e ->
      (try Dom.removeChild Dom_html.document##.body (Dom_html.getElementById "dialogue")
      with _ -> ());
      let current_wd_list = List.map (fun x -> x#get_value) (React.S.value grid#s_items) in
      let widgets  = wd_list current_wd_list in
      let dialogue = new Dialog.t
                       ~title:"What widget you'd like to add?"
                       ~content:(`Widgets widgets)
                       ~actions:[ new Dialog.Action.t ~typ:`Decline ~label:"Decline" ()
                                ; new Dialog.Action.t ~typ:`Accept  ~label:"Accept"  ()
                       ] ()
      in
      dialogue#root##.id := Js.string "dialogue";
      Dom.appendChild Dom_html.document##.body dialogue#root;
      let open Lwt.Infix in
      Dom_html.stopPropagation e;
      Lwt.bind dialogue#show_await
        (function
         | `Accept ->
            List.iter (fun item -> item#remove) (React.S.value grid#s_items);
            let chosen_buttons =
              List.filter (fun x -> x#get_input_widget#get_checked) widgets in
            let (chosen_widgets: (string * Wm.widget) list) =
              List.fold_left
                (fun acc x ->
                  let id = x#get_input_widget#get_id in
                  let w  = CCList.find_pred (fun (x: (string * Wm.widget)) -> id = (fst x))
                             wm.widgets in
                  match w with
                  | Some w -> w :: acc
                  | None   -> acc)
                [] chosen_buttons in
            let res_x, res_y = wm.resolution in
            let grid_asp = float_of_int res_x /. float_of_int res_y in
            let cont_asp = 4. /. 3. in
            let opt_cols, opt_rows = calc_rows_cols grid_asp cont_asp chosen_widgets in
            List.iteri (fun i el ->
                let w = cols / opt_cols in
                let h = rows / opt_rows in
                let row_num = i / opt_cols in
                let x = (i - row_num * opt_cols) * w in
                let y = row_num * h in
                grid#add (Dynamic_grid.Item.to_item ~pos:{x;y;w;h} ~value:el ())
                |> (function
                    | Ok _    -> print_endline "grid - add okay!";
                                 Lwt.return_unit
                     | Error _ -> print_endline "grid - add error!";
                                  Lwt.return_unit)
                |> ignore) chosen_widgets;
            Lwt.return ()
         | `Cancel ->
            print_endline "Dialog cancelled";
            Lwt.return ()))
    add_auto#e_click
  |> ignore;
  let demo = add[(section "Dynamic grid" [grid#widget; add_auto#widget; remove#widget])] in
  Dom.appendChild d demo;
  React.S.map (fun _ ->
      let layout =
        List.map (fun (x: 'a Dynamic_grid.Item.t) ->
            let wd = x#get_value in
            let width, height =
              match grid#root##.offsetWidth, grid#root##.offsetHeight with
              | 0,0 -> 100, 100
              | x,y -> x  , y
            in
            let col = width / props.cols in
            let row = match props.rows with
              | Some rows -> height / rows
              | None      -> 1
            in
            let str, wd1 = wd in
            let a, b = wd1.aspect in
            let wid_w, wid_h =
              if x#pos.w * col / a * b > x#pos.h * row
              then x#pos.h * row / b * a, x#pos.h * row
              else x#pos.w * col, x#pos.w * col / a * b
            in
            let real_w, real_h = wm.resolution in
            let (container_pos : Wm.position) =
              { left   = x#pos.x * col * real_w / width
              ; right  = (x#pos.x + x#pos.w) * col * real_w / width
              ; top    = (x#pos.y * row) * real_h / height
              ; bottom = (x#pos.y + x#pos.h) * row * real_h / height }
            in
            let left =
              container_pos.left   + (x#pos.w * col - wid_w) / 2 * real_w / width in
            let right =
              container_pos.right  - (x#pos.w * col - wid_w) / 2 * real_w / width in
            let top =
              container_pos.top    + (x#pos.h * row - wid_h) / 2 * real_h / height in
            let bottom =
              container_pos.bottom - (x#pos.h * row - wid_h) / 2 * real_h / height in
            let (widget_pos : Wm.position) = { left; right; top; bottom } in
            let wd1 = {wd1 with position = widget_pos } in
            str,
            ({ position = container_pos
             ; widgets  = [str,wd1]}: Wm.container)
          ) (React.S.value grid#s_items) in
      (layout)) grid#s_change

