open Components
open React
open Tyxml_js

let js = Js.string

let resolution_to_aspect resolution =
  let w, h = resolution in
  let rec deja_vu a b =
    if a != 0 && b != 0 then
      let a, b =
      if a > b
      then a mod b, b
      else a, b mod a
      in
      deja_vu a b
    else a + b in
  let divisor = deja_vu w h in
  let new_res = w / divisor, h / divisor in
  new_res


let section ?(style = "") ?(classes = []) title content =
  List.iter (fun x -> x#style##.margin := Js.string "10px") content;
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
    | 256, 135 -> 256, 135 (*UHD N*K*)        (*!!!*)
    | _        -> 30 , 20
  in
  let wd_list = List.fold_left (fun acc (x: (string * Wm.widget)) ->
                    let str, wd = x in
                    let label = str ^ " " ^ wd.description in
                    let radio = new Radio.t ~name:"radio" ~value:x () in
                    let form_field =
                      new Form_field.t ~label ~input:radio () in
                    List.append acc [form_field]
                  ) [] wm.widgets in
  let dialogue = new Dialog.t
                   ~title:"What widget u'd like to add?"
                   ~content:(`Widgets wd_list)
                   ~actions:[ new Dialog.Action.t ~typ:`Decline ~label:"Decline" ()
                            ; new Dialog.Action.t ~typ:`Accept  ~label:"Accept"  ()
                            ]
                   () in
  Dom.appendChild Dom_html.document##.body dialogue#root;
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
        let _, cont = x in
        let res_w, res_h = wm.resolution in
        let x = cols * cont.position.left / res_w in
        let w = cols * (cont.position.right - cont.position.left) / res_w in
        let y = rows * cont.position.top / res_h in
        let h = rows * (cont.position.bottom - cont.position.top) / res_h in
        Dynamic_grid.Item.to_item
          ~pos:{ x; y; w; h }
          ~min_w:1
          ~min_h:1
          ~value:(List.hd cont.widgets) ()
      ) wm.layout in
  let add_free = new Button.t ~label:"add" () in
  let grid  = new Dynamic_grid.t ~grid:props ~items () in
  React.E.map (fun e -> let open Lwt.Infix in
                        Dom_html.stopPropagation e;
                         Lwt.bind dialogue#show_await
                           (function
                            | `Accept ->
                               let chosen_wdg =
                                 (List.find
                                    (fun x -> x#get_input_widget#get_checked)
                                    wd_list)#get_input_widget#get_value in
                               grid#add_free ~min_w:1 ~min_h:1 ~value:chosen_wdg ()
                               >>= (function
                                    | Ok _    -> Lwt.return_unit
                                    | Error _ -> Lwt.return_unit)
                               |> ignore;
                               Lwt.return ()
                            | `Cancel -> print_endline "Dialog cancelled"; Lwt.return ()))
    add_free#e_click
  |> ignore;
  let demo = add[(section "Dynamic grid" [grid#widget; add_free#widget])] in
  Dom.appendChild d demo;
  React.S.map (fun _ ->
      let layout =
        List.map (fun (x: 'a Dynamic_grid.Item.t) ->
            let wd = x#get_value in
            let width, height = grid#root##.offsetWidth, grid#root##.offsetHeight in
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

