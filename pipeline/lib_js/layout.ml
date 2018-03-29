open Containers
open Components
open React
open Tyxml_js

(* returns true if el matches any part of the list*)
let refers_to_any (el: string * Wm.widget) (list: (string * Wm.widget) list) =
  List.fold_while (fun acc x ->
      (* TODO reconsider this *)
      if String.equal (fst x) (fst el) then (true, `Stop) else (acc, `Continue)) false list

(* a function that *)
let resolution_to_aspect resolution =
  let w, h = resolution in
  let rec greatest_common_divisor a b =
    if a <> 0 && b <> 0 then
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
        let rows = rows + 1 in
        let cols = ceil (float_of_int num /. float_of_int rows) in
        let w = 1. in
        let h = w /. grid_asp in
        if Float.((w /. cols *. cont_asp) *. float_of_int rows <= h)
        then int_of_float cols, rows, (w /. cols *. w /. cols *. cont_asp)
        else int_of_float cols, rows,
             (h /. (float_of_int rows) *. h /. (float_of_int rows) *. cont_asp)
      ) list in
  let (cols:int), (rows:int), _ =
    List.fold_left (fun acc x ->
        let _,_,sq = x in
        let _,_,gr = acc in
        if Float.(gr > sq) then acc else x)
                   (0,0,0.) squares in
  cols, rows

let initialize (wm: Wm.t) =
  let asp  = resolution_to_aspect wm.resolution in
  let cols, rows = 30,40
    (* let open Dynamic_grid.Utils in
     * if fst asp <= 40
     * then
     *   let weight = round (30. /. (float_of_int @@ fst asp)) in
     *   weight * (fst asp), weight * (snd asp)
     * else
     *   let weight = round (float_of_int (fst asp) /. 30.) in  (\* not proper but okay i guess *\)
     *   (fst asp) / weight, (snd asp) / weight *)
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
    ; items_margin     = (2,2)
    ; multi_select     = false
    ; restrict_move    = false
    } in
  let (items:'a Dynamic_grid.item list) =
    List.map
      (fun (x: string * Wm.container) ->
        let _, cont      = x in
        let left, right, top, bottom =
          float_of_int cont.position.left, float_of_int cont.position.right,
          float_of_int cont.position.top, float_of_int cont.position.bottom in
        let res_w, res_h = wm.resolution in
        let res_w, res_h = float_of_int res_w, float_of_int res_h in
        let x            = int_of_float @@ ceil @@ float_of_int cols *. (left /. res_w) in
        let w            = int_of_float @@ ceil @@ float_of_int cols *. ((right -. left) /. res_w) in
        let y            = int_of_float @@ ceil @@ float_of_int rows *. (top /. res_h) in
        let h            = int_of_float @@ ceil @@ float_of_int rows *. ((bottom -. top) /. res_h) in
        let value        =
          match cont.widgets with
          | [] -> List.hd wm.widgets
          | _  -> List.hd cont.widgets
        in
        Dynamic_grid.Item.to_item
          ~pos:{ x; y; w; h }
          ~min_w:1
          ~min_h:1
          ~selectable:true
          ~value ()
      ) wm.layout in

  let grid = new Dynamic_grid.t ~grid:props ~items () in

  let f_add = React.E.map (fun e ->
                  (try Dom.removeChild Dom_html.document##.body (Dom_html.getElementById "dialogue")
                   with _ -> ());
                  let current_wd_list = List.map (fun x -> x#get_value) (React.S.value grid#s_items) in
                  let widgets  = wd_list current_wd_list in
                  let dialogue = new Dialog.t
                                     ~title:"What widget u'd like to add?"
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
                                     let w  = List.find_pred
                                                (fun (x: (string * Wm.widget)) ->
                                                  String.equal id (fst x))
                                                wm.widgets in
                                     match w with
                                     | Some w -> w :: acc
                                     | None   -> acc)
                                   [] chosen_buttons in
                               let res_x, res_y = wm.resolution in
                               let grid_asp = float_of_int res_x /. float_of_int res_y in
                               let cont_asp = 16. /. 9. in
                               let opt_cols, opt_rows =
                                 calc_rows_cols grid_asp cont_asp chosen_widgets in
                               List.iteri (fun i el ->
                                   let w = cols / opt_cols in
                                   let h = rows / opt_rows  in
                                   let row_num = i / opt_cols in
                                   let x = (i - opt_cols * row_num) * w in
                                   let y = row_num * h in
                                   grid#add (Dynamic_grid.Item.to_item ~pos:{x;y;w;h} ~selectable:true ~value:el ())
                                   |> ignore) chosen_widgets;
                               Lwt.return ()
                            | `Cancel -> Lwt.return ()))
  in
  let s = React.S.map (fun _ ->
              let layout =
                List.map (fun (x: 'a Dynamic_grid.Item.t) ->
                    let width, height =
                      match grid#root##.offsetWidth, grid#root##.offsetHeight with
                      | 0,0 -> 640, 360
                      | x,y -> x  , y
                    in
                    let col = width / props.cols in
                    let row = match props.rows with
                      | Some rows -> height / rows
                      | None      -> 1
                    in
                    let str, wd = x#get_value in
                    let a, b = wd.aspect in
                    let a, b =
                      match a, b with
                      | 0, _ | _, 0 -> 1,1
                      | _, _ -> a, b
                    in
                    let wid_w, wid_h =
                      if x#pos.w * col / a > x#pos.h * row / b
                      then x#pos.h * row / b * a, x#pos.h * row
                      else x#pos.w * col, x#pos.w * col / a * b
                    in
                    let real_w, real_h =
                      match wm.resolution with
                      | 0, 0 -> 1, 1
                      | _, _ -> wm.resolution
                    in
                    let (container_pos : Wm.position) =
                      { left   = x#pos.x * col * real_w / width
                      ; right  = (x#pos.x + x#pos.w) * col * real_w / width
                      ; top    = (x#pos.y * row) * real_h / height
                      ; bottom = (x#pos.y + x#pos.h) * row * real_h / height }
                    in
                    let left   = container_pos.left   + (x#pos.w * col - wid_w) * real_w / width / 2 in
                    let right  = container_pos.right  - (x#pos.w * col - wid_w) * real_w / width / 2 in
                    let top    = container_pos.top    + (x#pos.h * row - wid_h) * real_h / height / 2 in
                    let bottom = container_pos.bottom - (x#pos.h * row - wid_h) * real_h / height / 2 in
                    let (widget_pos : Wm.position) = { left; right; top; bottom } in
                    let wd1 = {wd with position = widget_pos } in
                    str,
                    ({ position = container_pos
                     ; widgets  = [str,wd1]}: Wm.container)
                  ) (React.S.value grid#s_items) in
              (layout)) grid#s_change
  in
  grid,s,f_add
